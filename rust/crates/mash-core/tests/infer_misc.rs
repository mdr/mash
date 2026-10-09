//! Ports of BindTypesTest, ValueTypeDetectorTest and SimpleEvaluatorTest.

use std::rc::Rc;

use mash_core::ast::{E, ExprKind, NodeIdGen, SourceUnit};
use mash_core::builtins::maths::STATS_CLASS;
use mash_core::builtins::time::DAYS_CLASS;
use mash_core::compile::{self, Settings};
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::function::{Param, Params};
use mash_core::infer::{self, TypeDetector, TypedArgs, ValueInfo};
use mash_core::platform::TestPlatform;
use mash_core::value::{ClassRef, DateTime, LocalDate, Object, Str, Value};

// ------------------------------------------------------------------------------------------------ BindTypesTest

/// For each argument of the (first) invocation in `s`, the name of the parameter it binds to
fn arg_params(params: &Params, s: &str) -> Vec<Option<String>> {
    let compiled = compile::compile_forgiving(
        SourceUnit::new("test", s),
        Settings::default(),
        &Default::default(),
        &mut NodeIdGen::default(),
    );
    let ExprKind::Invocation { args, .. } = &compiled.program.body.kind else {
        panic!("not an invocation: {s}")
    };
    let typed = TypedArgs {
        args: args
            .iter()
            .map(|arg| match arg {
                mash_core::ast::Arg::Positional(_) => infer::TypedArg::Positional(ValueInfo::default()),
                mash_core::ast::Arg::ShortFlags { flags, .. } => infer::TypedArg::ShortFlags(flags.clone()),
                mash_core::ast::Arg::LongFlag { name, value, .. } => {
                    infer::TypedArg::LongFlag(name.clone(), value.as_ref().map(|_| ValueInfo::default()))
                }
            })
            .collect(),
    };
    let bound = typed.bind(params);
    bound
        .arg_params
        .iter()
        .map(|p| p.map(|i| params.params[i].name().to_string()))
        .collect()
}

fn names(names: &[&str]) -> Vec<Option<String>> {
    names.iter().map(|n| Some(n.to_string())).collect()
}

#[test]
fn bind_types() {
    let one = Params::new(vec![Param::named("param")]);
    assert_eq!(arg_params(&one, "f 1"), names(&["param"]));
    let three = Params::new(vec![
        Param::named("param1"),
        Param::named("param2"),
        Param::named("param3"),
    ]);
    assert_eq!(arg_params(&three, "f 1 2 3"), names(&["param1", "param2", "param3"]));
    let variadic = Params::new(vec![Param::named("param").variadic()]);
    assert_eq!(arg_params(&variadic, "f 1 2 3"), names(&["param", "param", "param"]));
    let extra = Params::new(vec![Param::named("param1"), Param::named("param2").variadic()]);
    assert_eq!(arg_params(&extra, "f 1 2 3"), names(&["param1", "param2", "param2"]));
    assert_eq!(arg_params(&one, "f --param=1"), names(&["param"]));
    let two = Params::new(vec![Param::named("param1"), Param::named("param2")]);
    assert_eq!(
        arg_params(&two, "f --param2=1 --param1=2"),
        names(&["param2", "param1"])
    );
    let last_variadic = Params::new(vec![Param::named("param1").variadic(), Param::named("param2")]);
    assert_eq!(
        arg_params(&last_variadic, "f 1 2 3"),
        names(&["param1", "param1", "param2"])
    );
    let optional_first = Params::new(vec![
        Param::named("param1").default_value(|| Value::Bool(true)),
        Param::named("param2"),
    ]);
    assert_eq!(arg_params(&optional_first, "f 1"), names(&["param2"]));
    assert_eq!(
        arg_params(&optional_first, "f 1 --param2=2"),
        names(&["param1", "param2"])
    );
    let all = Params::new(vec![
        Param::named("positional"),
        Param::named("optional").default_value(|| Value::Bool(true)),
        Param::named("variadic").variadic(),
        Param::named("last"),
        Param::named("named").flag(),
    ]);
    assert_eq!(
        arg_params(&all, "f 1 2 3 4 5 --named=5"),
        names(&["positional", "optional", "variadic", "variadic", "last", "named"])
    );
}

// ------------------------------------------------------------------------------------------------ ValueTypeDetectorTest

fn type_of(value: Value) -> String {
    format!("{:?}", TypeDetector::default().type_of(&value))
}

#[test]
fn value_types() {
    assert_eq!(type_of(Value::Bool(true)), "core.Boolean");
    assert_eq!(type_of(Value::Unit), "core.Unit");
    assert_eq!(type_of(Value::Null), "core.Null");
    assert_eq!(
        type_of(Value::DateTime(DateTime(jiff::Timestamp::UNIX_EPOCH))),
        "time.DateTime"
    );
    assert_eq!(
        type_of(Value::Date(LocalDate(jiff::civil::date(2026, 10, 9)))),
        "time.Date"
    );
    assert_eq!(type_of(Value::str("foo")), "core.String");
    assert_eq!(
        type_of(Value::Str(Str::tagged("foo", &mash_core::builtins::os::PATH_CLASS))),
        "core.String tagged os.Path"
    );
    assert_eq!(type_of(Value::num(1.0)), "core.Number");
    assert_eq!(
        type_of(Value::Number(1.0.into(), Some(&DAYS_CLASS))),
        "core.Number tagged time.Days"
    );
    assert_eq!(
        type_of(Value::Object(Object::from_pairs([("foo", Value::Bool(true))]))),
        "{foo: core.Boolean}"
    );
    // (Scala used os.PermissionsSection, an M5 class)
    let stats = Object::with_class(Some(ClassRef::Builtin(&STATS_CLASS)));
    stats.set_str("min", Value::num(1.0));
    assert_eq!(type_of(Value::Object(stats)), "maths.Stats");
}

// ------------------------------------------------------------------------------------------------ SimpleEvaluatorTest

/// Constant folding gives the same value as evaluation
#[test]
fn constant_folding() {
    with_big_stack(|| {
        for s in [
            "42",
            "'foo'",
            "[1, 2, 3]",
            "[1, 2, 3][1]",
            "{ foo: 42 }",
            "{ foo: 42 }.foo",
        ] {
            let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
            let compiled = ctx.compile_forgiving(SourceUnit::new("test", s));
            let body: &E = &compiled.program.body;
            let inferred = infer::infer(&ctx, body);
            let constant = inferred.constants.get(&body.id).cloned();
            let evaluated = ctx.eval_program(&compiled.program).unwrap();
            assert_eq!(constant, Some(evaluated), "{s}");
        }
    })
}
