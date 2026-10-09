//! Attribute macros for defining Mash builtins from ordinary Rust functions.
//!
//! ```ignore
//! /// Find the length of a sequence
//! ///
//! /// Examples:
//! /// <mash>
//! ///   length [1, 2, 3] # 3
//! /// </mash>
//! #[builtin(namespace = "collections")]
//! fn length(#[param("Sequence to find the length of")] sequence: Sequence) -> R<usize> {
//!     Ok(sequence.len())
//! }
//! ```
//!
//! generates a unit struct `LengthFunction` implementing `BuiltinFunction`, and keeps the function itself.
//!
//! * The doc comment gives the summary (first paragraph) and description (the rest).
//! * The Mash name is the function's name in camelCase (`sort_by` → `sortBy`), unless given with `name = "…"`.
//! * Each parameter's type decides how its argument is extracted: `Value` as it is; `Option<T>` makes the
//!   parameter optional (absent → `None`); any other `T: FromValue` is converted, with an "Invalid argument"
//!   error on failure. Parameters of type `&mut Ctx` and `&Args` receive the interpreter and the raw arguments
//!   (an escape hatch) and are not Mash parameters.
//! * `#[param(…)]` on a parameter configures it: a string literal is its summary; also `short = 'c'`, `flag`,
//!   `boolean_flag`, `default = expr` (converted with `Value::from`), `optional`, `variadic`, `at_least_one`,
//!   `flatten`, `last`, `lazy`, `named_args`, `all_args`, `safe`, `name = "…"`, `description = "…"`,
//!   `flag_value_name = "…"`, `mandatory_flag_value`, and `def = path` to start from a shared parameter
//!   (`path()` returns a `Param`).
//! * The function returns `R<T>` for any `T` convertible into `Value`.
//!
//! `#[builtin_method]` is the same for methods of builtin classes: the parameter named `target` receives the
//! object the method is called on (as `&Value`, or converted like other parameters), and the struct is named
//! `…Method`. It also takes `aliases = ["…"]`, `shy` and `private`. `#[builtin]` takes `aliases = ["ns.name"]`
//! (alternative full names).
//!
//! For type inference, either `returns = <expr>` (an expression of type `Type`, for a fixed result type) or
//! `infer = path`: for a function, `fn(&mut Inferencer, &Params, &TypedArgs) -> Option<Type>`; for a method,
//! `fn(&mut Inferencer, Option<&Type>, &Params, &TypedArgs) -> Option<Type>` (the `Option<&Type>` is the
//! target's type).
//!
//! For completion of arguments, `completion = path`: for a function, `fn(&Params, usize, &TypedArgs) ->
//! Vec<CompletionSpec>`; for a method, `fn(&Params, usize, Option<&Type>, &TypedArgs) -> Vec<CompletionSpec>`
//! (the `usize` is the position of the argument being completed).

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    Attribute, Expr, ExprLit, FnArg, GenericArgument, Ident, ItemFn, Lit, LitStr, Meta, Pat, PathArguments, Token,
    Type, parse_macro_input,
};

#[proc_macro_attribute]
pub fn builtin(attr: TokenStream, item: TokenStream) -> TokenStream {
    let options = parse_macro_input!(attr as Options);
    let function = parse_macro_input!(item as ItemFn);
    expand(options, function, Kind::Function)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_attribute]
pub fn builtin_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let options = parse_macro_input!(attr as Options);
    let function = parse_macro_input!(item as ItemFn);
    expand(options, function, Kind::Method)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Function,
    Method,
}

/// Options in `#[builtin(…)]` / `#[builtin_method(…)]`
#[derive(Default)]
struct Options {
    name: Option<LitStr>,
    namespace: Option<LitStr>,
    aliases: Vec<LitStr>,
    shy: bool,
    private: bool,
    /// The result type, as an expression of type `Type`
    returns: Option<Expr>,
    /// A function inferring the result type
    infer: Option<Expr>,
    /// A function giving completion specs for arguments
    completion: Option<Expr>,
}

impl Parse for Options {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut options = Options::default();
        for meta in Punctuated::<Meta, Token![,]>::parse_terminated(input)? {
            let key = meta.path().get_ident().map(Ident::to_string).unwrap_or_default();
            match (key.as_str(), &meta) {
                ("name", Meta::NameValue(nv)) => options.name = Some(lit_str(&nv.value)?),
                ("namespace", Meta::NameValue(nv)) => options.namespace = Some(lit_str(&nv.value)?),
                ("aliases", Meta::NameValue(nv)) => options.aliases = lit_strs(&nv.value)?,
                ("returns", Meta::NameValue(nv)) => options.returns = Some(nv.value.clone()),
                ("infer", Meta::NameValue(nv)) => options.infer = Some(nv.value.clone()),
                ("completion", Meta::NameValue(nv)) => options.completion = Some(nv.value.clone()),
                ("shy", Meta::Path(_)) => options.shy = true,
                ("private", Meta::Path(_)) => options.private = true,
                _ => return Err(syn::Error::new(meta.span(), "unknown builtin option")),
            }
        }
        Ok(options)
    }
}

fn lit_str(expr: &Expr) -> syn::Result<LitStr> {
    match expr {
        Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => Ok(s.clone()),
        _ => Err(syn::Error::new(expr.span(), "expected a string literal")),
    }
}

fn lit_strs(expr: &Expr) -> syn::Result<Vec<LitStr>> {
    match expr {
        Expr::Array(array) => array.elems.iter().map(lit_str).collect(),
        _ => Err(syn::Error::new(expr.span(), "expected an array of string literals")),
    }
}

/// How a Rust parameter receives its value
enum Source {
    Ctx,
    Args,
    Target {
        by_ref: bool,
    },
    /// A Mash parameter, with its index in the parameter list
    Param {
        name: String,
        extraction: Extraction,
    },
}

enum Extraction {
    Value,
    Option,
    Convert,
}

fn expand(options: Options, mut function: ItemFn, kind: Kind) -> syn::Result<TokenStream2> {
    let fn_name = function.sig.ident.clone();
    let mash_name = options
        .name
        .as_ref()
        .map(LitStr::value)
        .unwrap_or_else(|| camel_case(&fn_name.to_string()));
    let struct_name = format_ident!(
        "{}{}",
        pascal_case(&fn_name.to_string()),
        if kind == Kind::Function { "Function" } else { "Method" }
    );
    let (summary, description) = doc_comment(&function.attrs);
    let summary = summary.ok_or_else(|| syn::Error::new(fn_name.span(), "builtins need a doc comment"))?;

    let mut param_builders = vec![];
    let mut call_args = vec![];
    for input in function.sig.inputs.iter_mut() {
        let FnArg::Typed(arg) = input else {
            return Err(syn::Error::new(input.span(), "builtins can't take self"));
        };
        let Pat::Ident(pat) = &*arg.pat else {
            return Err(syn::Error::new(arg.pat.span(), "expected a parameter name"));
        };
        let rust_name = pat.ident.to_string();
        let param_attrs: Vec<Attribute> = arg
            .attrs
            .iter()
            .filter(|a| a.path().is_ident("param"))
            .cloned()
            .collect();
        arg.attrs.retain(|a| !a.path().is_ident("param"));
        let source = if is_ref_to(&arg.ty, "Ctx", true) {
            Source::Ctx
        } else if is_ref_to(&arg.ty, "Args", false) {
            Source::Args
        } else if kind == Kind::Method && rust_name == "target" {
            Source::Target {
                by_ref: matches!(&*arg.ty, Type::Reference(_)),
            }
        } else {
            let (builder, name) = param_builder(&rust_name, &arg.ty, &param_attrs)?;
            param_builders.push(builder);
            let extraction = if is_named(&arg.ty, "Value") {
                Extraction::Value
            } else if option_inner(&arg.ty).is_some() {
                Extraction::Option
            } else {
                Extraction::Convert
            };
            Source::Param { name, extraction }
        };
        if !param_attrs.is_empty() && !matches!(source, Source::Param { .. }) {
            return Err(syn::Error::new(arg.span(), "#[param] only applies to Mash parameters"));
        }
        call_args.push(match source {
            Source::Ctx => quote!(ctx),
            Source::Args => quote!(args),
            Source::Target { by_ref: true } => quote!(target),
            Source::Target { by_ref: false } => quote! {
                ::mash_core::function::FromValue::from_value(target)
                    .map_err(|message| ::mash_core::eval::MashError::new(message))?
            },
            Source::Param {
                name,
                extraction: Extraction::Value,
            } => quote!(args.value(#name).clone()),
            Source::Param {
                name,
                extraction: Extraction::Option,
            } => quote!(args.get_opt(#name)?),
            Source::Param {
                name,
                extraction: Extraction::Convert,
            } => quote!(args.get(#name)?),
        });
    }

    let description = match description {
        Some(d) => quote!(Some(#d)),
        None => quote!(None),
    };
    let params = quote! {
        fn params(&self) -> &::mash_core::function::Params {
            static PARAMS: ::std::sync::LazyLock<::mash_core::function::Params> =
                ::std::sync::LazyLock::new(|| ::mash_core::function::Params::new(vec![#(#param_builders),*]));
            &PARAMS
        }
    };
    if options.returns.is_some() && options.infer.is_some() {
        return Err(syn::Error::new(
            Span::call_site(),
            "use either returns or infer, not both",
        ));
    }
    let infer = match (&options.returns, &options.infer, kind) {
        (Some(ty), _, Kind::Function) => quote! {
            fn infer(&self, _: &mut ::mash_core::infer::Inferencer<'_>, _: &::mash_core::infer::TypedArgs)
                -> Option<::mash_core::infer::Type> { Some(#ty) }
        },
        (Some(ty), _, Kind::Method) => quote! {
            fn infer(&self, _: &mut ::mash_core::infer::Inferencer<'_>, _: Option<&::mash_core::infer::Type>,
                _: &::mash_core::infer::TypedArgs) -> Option<::mash_core::infer::Type> { Some(#ty) }
        },
        (None, Some(f), Kind::Function) => quote! {
            fn infer(&self, inferencer: &mut ::mash_core::infer::Inferencer<'_>, args: &::mash_core::infer::TypedArgs)
                -> Option<::mash_core::infer::Type> { (#f)(inferencer, self.params(), args) }
        },
        (None, Some(f), Kind::Method) => quote! {
            fn infer(&self, inferencer: &mut ::mash_core::infer::Inferencer<'_>, target: Option<&::mash_core::infer::Type>,
                args: &::mash_core::infer::TypedArgs) -> Option<::mash_core::infer::Type> {
                (#f)(inferencer, target, self.params(), args)
            }
        },
        (None, None, _) => quote!(),
    };
    let completion = match (&options.completion, kind) {
        (Some(f), Kind::Function) => quote! {
            fn completion_specs(&self, arg_pos: usize, args: &::mash_core::infer::TypedArgs)
                -> Vec<::mash_core::complete::CompletionSpec> { (#f)(self.params(), arg_pos, args) }
        },
        (Some(f), Kind::Method) => quote! {
            fn completion_specs(&self, arg_pos: usize, target: Option<&::mash_core::infer::Type>,
                args: &::mash_core::infer::TypedArgs) -> Vec<::mash_core::complete::CompletionSpec> {
                (#f)(self.params(), arg_pos, target, args)
            }
        },
        (None, _) => quote!(),
    };
    let aliases = &options.aliases;
    let vis = &function.vis;
    let doc = format!("The builtin `{mash_name}`.");
    let generated = match kind {
        Kind::Function => {
            let namespace = options
                .namespace
                .ok_or_else(|| syn::Error::new(Span::call_site(), "#[builtin] needs namespace = \"…\""))?;
            quote! {
                #[doc = #doc]
                #[derive(Debug)]
                #vis struct #struct_name;

                impl ::mash_core::function::BuiltinFunction for #struct_name {
                    fn name(&self) -> &'static str { #mash_name }
                    fn namespace(&self) -> &'static str { #namespace }
                    fn aliases(&self) -> &'static [&'static str] { &[#(#aliases),*] }
                    #params
                    #[allow(unused_variables)]
                    fn call(
                        &self,
                        ctx: &mut ::mash_core::eval::Ctx,
                        args: &::mash_core::function::Args,
                    ) -> ::mash_core::eval::R<::mash_core::value::Value> {
                        #fn_name(#(#call_args),*).map(::mash_core::value::Value::from)
                    }
                    fn summary(&self) -> &'static str { #summary }
                    fn description(&self) -> Option<&'static str> { #description }
                    #infer
                    #completion
                }
            }
        }
        Kind::Method => {
            if options.namespace.is_some() {
                return Err(syn::Error::new(Span::call_site(), "methods don't have a namespace"));
            }
            let (shy, private) = (options.shy, options.private);
            quote! {
                #[doc = #doc]
                #[derive(Debug)]
                #vis struct #struct_name;

                impl ::mash_core::function::BuiltinMethod for #struct_name {
                    fn name(&self) -> &'static str { #mash_name }
                    fn aliases(&self) -> &'static [&'static str] { &[#(#aliases),*] }
                    #params
                    #[allow(unused_variables)]
                    fn call(
                        &self,
                        ctx: &mut ::mash_core::eval::Ctx,
                        target: &::mash_core::value::Value,
                        args: &::mash_core::function::Args,
                    ) -> ::mash_core::eval::R<::mash_core::value::Value> {
                        #fn_name(#(#call_args),*).map(::mash_core::value::Value::from)
                    }
                    fn summary(&self) -> &'static str { #summary }
                    fn description(&self) -> Option<&'static str> { #description }
                    fn shy(&self) -> bool { #shy }
                    fn private(&self) -> bool { #private }
                    #infer
                    #completion
                }
            }
        }
    };
    Ok(quote! {
        #function
        #generated
    })
}

/// The `Param` builder expression for a Mash parameter, and its Mash name
fn param_builder(rust_name: &str, ty: &Type, attrs: &[Attribute]) -> syn::Result<(TokenStream2, String)> {
    let mut name = camel_case(rust_name.trim_start_matches("r#").trim_end_matches('_'));
    let mut base = None;
    let mut modifiers = vec![];
    let mut has_default = false;
    for attr in attrs {
        let metas = attr.parse_args_with(Punctuated::<ParamOption, Token![,]>::parse_terminated)?;
        for option in metas {
            match option {
                ParamOption::Summary(s) => modifiers.push(quote!(.summary(#s))),
                ParamOption::Flag(key) => {
                    let method = Ident::new(&key, Span::call_site());
                    if key == "optional" || key == "boolean_flag" {
                        has_default = true;
                    }
                    modifiers.push(quote!(.#method()));
                }
                ParamOption::KeyValue(key, value) => match key.as_str() {
                    "name" => name = lit_str(&value)?.value(),
                    "def" => base = Some(value),
                    "default" => {
                        has_default = true;
                        modifiers.push(quote!(.default_value(|| ::mash_core::value::Value::from(#value))))
                    }
                    "short" | "description" | "flag_value_name" => {
                        let method = Ident::new(&key, Span::call_site());
                        modifiers.push(quote!(.#method(#value)))
                    }
                    _ => return Err(syn::Error::new(value.span(), format!("unknown parameter option {key}"))),
                },
            }
        }
    }
    if option_inner(ty).is_some() && !has_default {
        modifiers.push(quote!(.optional()));
    }
    let start = match base {
        Some(path) => quote!({ let mut p: ::mash_core::function::Param = (#path)(); p.name = Some(#name.into()); p }),
        None => quote!(::mash_core::function::Param::named(#name)),
    };
    Ok((quote!(#start #(#modifiers)*), name))
}

enum ParamOption {
    Summary(LitStr),
    Flag(String),
    KeyValue(String, Expr),
}

impl Parse for ParamOption {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            return Ok(ParamOption::Summary(input.parse()?));
        }
        let key: Ident = input.parse()?;
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Ok(ParamOption::KeyValue(key.to_string(), input.parse()?))
        } else {
            const FLAGS: &[&str] = &[
                "flag",
                "boolean_flag",
                "optional",
                "variadic",
                "at_least_one",
                "flatten",
                "last",
                "lazy",
                "named_args",
                "all_args",
                "safe",
                "mandatory_flag_value",
            ];
            let key = key.to_string();
            if !FLAGS.contains(&key.as_str()) {
                return Err(syn::Error::new(input.span(), format!("unknown parameter option {key}")));
            }
            Ok(ParamOption::Flag(key))
        }
    }
}

/// Whether `ty` is `&T` (or `&mut T` if `mutable`) where T's last path segment is `name`
fn is_ref_to(ty: &Type, name: &str, mutable: bool) -> bool {
    match ty {
        Type::Reference(r) => r.mutability.is_some() == mutable && is_named(&r.elem, name),
        _ => false,
    }
}

fn is_named(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Path(p) => p.path.segments.last().is_some_and(|s| s.ident == name),
        _ => false,
    }
}

fn option_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(p) = ty else { return None };
    let segment = p.path.segments.last()?;
    if segment.ident != "Option" {
        return None;
    }
    match &segment.arguments {
        PathArguments::AngleBracketed(args) => match args.args.first()? {
            GenericArgument::Type(t) => Some(t),
            _ => None,
        },
        _ => None,
    }
}

/// (summary, description) from `///` comments: the first paragraph, and the rest
fn doc_comment(attrs: &[Attribute]) -> (Option<String>, Option<String>) {
    let lines: Vec<String> = attrs
        .iter()
        .filter(|a| a.path().is_ident("doc"))
        .filter_map(|a| match &a.meta {
            Meta::NameValue(nv) => lit_str(&nv.value).ok().map(|s| s.value()),
            _ => None,
        })
        .map(|line| line.strip_prefix(' ').unwrap_or(&line).to_string())
        .collect();
    let mut paragraphs = lines.split(|line| line.trim().is_empty()).filter(|p| !p.is_empty());
    let summary = paragraphs.next().map(|p| p.join(" "));
    let rest_start = lines
        .iter()
        .position(|l| l.trim().is_empty())
        .map(|i| i + 1)
        .unwrap_or(lines.len());
    let rest = lines[rest_start..].join("\n").trim().to_string();
    (summary, (!rest.is_empty()).then_some(rest))
}

fn camel_case(snake: &str) -> String {
    let mut out = String::new();
    let mut upper = false;
    for c in snake.chars() {
        if c == '_' {
            upper = !out.is_empty();
        } else if upper {
            out.extend(c.to_uppercase());
            upper = false;
        } else {
            out.push(c);
        }
    }
    out
}

fn pascal_case(snake: &str) -> String {
    let camel = camel_case(snake);
    let mut chars = camel.chars();
    chars
        .next()
        .map(|c| c.to_uppercase().chain(chars).collect())
        .unwrap_or_default()
}
