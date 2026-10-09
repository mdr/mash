//! A compact, fully-parenthesised rendering of a tree, ignoring node ids and source ranges. For tests (two
//! trees are the same shape exactly when their renderings are equal) and debugging.

use std::fmt::Write;

use super::*;

pub fn pretty(e: &Expr) -> String {
    let mut out = String::new();
    expr(&mut out, e);
    out
}

fn expr(out: &mut String, e: &Expr) {
    use ExprKind as K;
    match &e.kind {
        K::Literal(Literal::Number(n)) => write!(out, "{n}").unwrap(),
        K::Literal(Literal::Bool(b)) => write!(out, "{b}").unwrap(),
        K::Literal(Literal::Null) => out.push_str("null"),
        K::String { value, quote, tilde } => {
            let q = if *quote == QuoteKind::Double { '"' } else { '\'' };
            write!(out, "{q}{}{value}{q}", if *tilde { "~" } else { "" }).unwrap()
        }
        K::Interpolated { parts, tilde } => {
            out.push_str(if *tilde { "(interpolated ~" } else { "(interpolated" });
            for part in parts {
                out.push(' ');
                match part {
                    InterpolationPart::Text(t) => write!(out, "{t:?}").unwrap(),
                    InterpolationPart::Expr(e) => expr(out, e),
                }
            }
            out.push(')')
        }
        K::Identifier(name) => out.push_str(name),
        K::Hole(n) => write!(out, "_{n}").unwrap(),
        K::HeadlessMember { name, null_safe, .. } => {
            write!(out, "({}{name})", if *null_safe { "?." } else { "." }).unwrap()
        }
        K::This => out.push_str("this"),
        K::MishFunction(command) => write!(out, "!{command}").unwrap(),
        K::Paren(e) => node(out, "paren", &[e]),
        K::Block(e) => node(out, "block", &[e]),
        K::StatementSeq(es) => list(out, "seq", es),
        K::List(es) => list(out, "list", es),
        K::Object(entries) => {
            out.push_str("(object");
            for entry in entries {
                out.push(' ');
                match entry {
                    ObjectEntry::Full { field, value } => {
                        out.push('(');
                        expr(out, field);
                        out.push_str(": ");
                        expr(out, value);
                        out.push(')');
                    }
                    ObjectEntry::Shorthand { name, .. } => out.push_str(name),
                }
            }
            out.push(')')
        }
        K::Member {
            target,
            name,
            null_safe,
            ..
        } => {
            out.push('(');
            expr(out, target);
            write!(out, "{}{name})", if *null_safe { "?." } else { "." }).unwrap()
        }
        K::Lookup { target, index } => node(out, "lookup", &[target, index]),
        K::Invocation {
            function,
            args: arguments,
            paren,
        } => {
            out.push_str(if *paren { "(call( " } else { "(call " });
            expr(out, function);
            args(out, arguments);
            out.push(')')
        }
        K::Minus(e) => node(out, "-", &[e]),
        K::BinOp { op, lhs, rhs } => node(out, op.symbol(), &[lhs, rhs]),
        K::ChainedOp { first, rest } => {
            out.push_str("(chain ");
            expr(out, first);
            for (op, e) in rest {
                write!(out, " {} ", op.symbol()).unwrap();
                expr(out, e);
            }
            out.push(')')
        }
        K::If { cond, then, else_ } => {
            let mut children = vec![cond, then];
            children.extend(else_);
            node(out, "if", &children)
        }
        K::Assignment { lhs, op, rhs } => {
            let op = op.map_or("=".to_string(), |op| format!("{}=", op.symbol()));
            node(out, &op, &[lhs, rhs])
        }
        K::PatternAssignment { pattern: p, rhs } => {
            out.push_str("(= ");
            pattern(out, p);
            out.push(' ');
            expr(out, rhs);
            out.push(')')
        }
        K::Lambda(def) => {
            out.push_str("(lambda ");
            fn_def(out, def);
            out.push(')')
        }
        K::FunctionDecl(def) => {
            out.push_str("(def ");
            fn_def(out, def);
            out.push(')')
        }
        K::ClassDecl(def) => {
            write!(out, "(class {}", def.name).unwrap();
            attributes(out, &def.attributes);
            params(out, &def.params);
            for method in &def.methods {
                out.push_str(" (def ");
                fn_def(out, method);
                out.push(')');
            }
            out.push(')')
        }
        K::Pipe { lhs, rhs } => node(out, "|", &[lhs, rhs]),
        K::Mish {
            command,
            args,
            redirects,
            capture_output,
        } => {
            out.push_str(if *capture_output { "(mish! " } else { "(mish " });
            expr(out, command);
            for arg in args {
                out.push(' ');
                expr(out, arg);
            }
            for redirect in redirects {
                out.push_str(if redirect.op == RedirectOp::StandardInput {
                    " <"
                } else {
                    " >"
                });
                expr(out, &redirect.arg);
            }
            out.push(')')
        }
        K::MishInterpolation(e) => node(out, "$", &[e]),
        K::Help(e) => node(out, "help", &[e]),
        K::Import { expr: e, name, .. } => {
            out.push_str("(import ");
            expr(out, e);
            write!(out, " {})", name.as_deref().unwrap_or("_")).unwrap()
        }
        K::Missing => out.push_str("<missing>"),
    }
}

fn node(out: &mut String, label: &str, children: &[&E]) {
    out.push('(');
    out.push_str(label);
    for child in children {
        out.push(' ');
        expr(out, child);
    }
    out.push(')')
}

fn list(out: &mut String, label: &str, children: &[E]) {
    node(out, label, &children.iter().collect::<Vec<_>>())
}

fn args(out: &mut String, args: &[Arg]) {
    for arg in args {
        out.push(' ');
        match arg {
            Arg::Positional(e) => expr(out, e),
            Arg::ShortFlags { flags, .. } => write!(out, "-{}", flags.iter().collect::<String>()).unwrap(),
            Arg::LongFlag { name, value, .. } => {
                write!(out, "--{name}").unwrap();
                if let Some(value) = value {
                    out.push('=');
                    expr(out, value);
                }
            }
        }
    }
}

fn attributes(out: &mut String, attributes: &[Attribute]) {
    for attribute in attributes {
        write!(out, " @{}", attribute.name).unwrap();
        if let Some(arguments) = &attribute.args {
            out.push('(');
            args(out, arguments);
            out.push_str(" )");
        }
    }
}

fn params(out: &mut String, params: &[Param]) {
    out.push_str(" [");
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        let mut param_attributes = String::new();
        attributes(&mut param_attributes, &param.attributes);
        if !param_attributes.is_empty() {
            out.push_str(param_attributes.trim_start());
            out.push(' ');
        }
        match (&param.name, &param.pattern) {
            (Some(name), None | Some(Pattern::Ident(_))) => out.push_str(name),
            (_, Some(p)) => pattern(out, p),
            (None, None) => out.push_str("<missing>"),
        }
        if param.variadic {
            out.push_str("...");
        }
        if let Some(default) = &param.default {
            out.push('=');
            expr(out, default);
        }
    }
    out.push(']')
}

fn fn_def(out: &mut String, def: &FnDef) {
    out.push_str(def.name.as_deref().unwrap_or("_"));
    attributes(out, &def.attributes);
    params(out, &def.params);
    out.push(' ');
    expr(out, &def.body)
}

fn pattern(out: &mut String, p: &Pattern) {
    match p {
        Pattern::Ident(name) => out.push_str(name),
        Pattern::Hole => out.push('_'),
        Pattern::Object(entries) => {
            out.push('{');
            for (i, entry) in entries.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&entry.field);
                if let Some(p) = &entry.pattern {
                    out.push_str(": ");
                    pattern(out, p);
                }
            }
            out.push('}')
        }
        Pattern::List(patterns) => {
            out.push('[');
            for (i, p) in patterns.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                pattern(out, p);
            }
            out.push(']')
        }
    }
}
