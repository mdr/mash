//! Evaluator behaviour beyond values: error locations and Mash stack traces, the call depth limit,
//! interruption, and garbage collection during evaluation.

use std::rc::Rc;
use std::sync::atomic::Ordering;

use mash_core::eval::{Ctx, MashError, RunError, with_big_stack};
use mash_core::platform::TestPlatform;
use mash_core::value::gc;

fn ctx() -> Ctx {
    Ctx::new(Rc::new(TestPlatform::default()))
}

fn eval_error(ctx: &mut Ctx, source: &str) -> MashError {
    match ctx.run("test", source) {
        Err(RunError::Eval(e)) => e,
        other => panic!("expected an evaluation error from {source:?}, got {other:?}"),
    }
}

#[test]
fn errors_have_a_location_and_a_mash_stack_trace() {
    with_big_stack(|| {
        let mut ctx = ctx();
        let source = "def inner = error 'boom'\ndef outer = inner\nouter";
        let error = eval_error(&mut ctx, source);
        assert_eq!(error.message, "boom");
        // Innermost first: the call of `error` in `inner`, the call of `inner` in `outer`, the call of `outer`
        let frames: Vec<(Option<&str>, Option<&str>)> = error
            .stack
            .iter()
            .map(|f| (f.location.as_ref().map(|l| l.snippet()), f.function.as_deref()))
            .collect();
        assert_eq!(
            frames,
            [
                (Some("error 'boom'"), Some("inner")),
                (Some("inner"), Some("outer")),
                (Some("outer"), None)
            ]
        );
    });
}

#[test]
fn member_errors_point_at_the_member_name() {
    with_big_stack(|| {
        let mut ctx = ctx();
        let error = eval_error(&mut ctx, "x = 42; x.nosuch");
        assert!(
            error
                .message
                .starts_with("Cannot find member 'nosuch' in value of type Number. Did you mean"),
            "{}",
            error.message
        );
        assert_eq!(error.location().map(|l| l.snippet()), Some("nosuch"));
    });
}

#[test]
fn argument_errors_point_at_the_argument_or_the_call() {
    with_big_stack(|| {
        let mut ctx = ctx();
        let error = eval_error(&mut ctx, "def f x = x; f 1 22");
        assert_eq!(
            error.message,
            "Too many positional arguments -- 2 were provided, but at most 1 is allowed"
        );
        assert_eq!(error.location().map(|l| l.snippet()), Some("22"));
        let error = eval_error(&mut ctx, "def f x = x; f --y=1");
        assert_eq!(error.message, "Unexpected named argument 'y'. Did you mean 'x'?");
        let error = eval_error(&mut ctx, "def f x y = x; (f 1)");
        assert_eq!(error.message, "Missing mandatory argument 'y'");
        assert_eq!(error.location().map(|l| l.snippet()), Some("f 1"));
        let error = eval_error(&mut ctx, "not()");
        assert_eq!(error.message, "Missing mandatory argument 'item'");
    });
}

#[test]
fn runaway_recursion_is_an_error() {
    with_big_stack(|| {
        let mut ctx = ctx();
        let error = eval_error(&mut ctx, "def f n = f (n + 1); f 0");
        assert!(
            error.message.starts_with("Maximum call depth (10000) exceeded"),
            "{}",
            error.message
        );
        // Recursion just within the limit is fine (and fits the stack in a debug build)
        let value = ctx
            .run("test", "def g n = if n == 0 then 0 else 1 + g (n - 1); g 9990")
            .unwrap();
        assert_eq!(format!("{value:?}"), "9990");
    });
}

#[test]
fn evaluation_can_be_interrupted() {
    with_big_stack(|| {
        let mut ctx = ctx();
        ctx.step_budget = Some(1000);
        let error = eval_error(&mut ctx, "def f n = f (n + 1); f 0");
        assert!(error.is_interrupted());
        ctx.step_budget = None;
        // The flag is cleared at the start of each run, so set it from "another thread" mid-run instead
        let flag = ctx.interrupt_flag();
        let setter = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(50));
            flag.store(true, Ordering::Relaxed);
        });
        let error = eval_error(&mut ctx, "def loop n = loop n; loop 0");
        setter.join().unwrap();
        // Either interrupted, or hit the depth limit first on a fast machine
        assert!(
            error.is_interrupted() || error.message.contains("depth"),
            "{}",
            error.message
        );
    });
}

#[test]
fn closures_created_in_a_loop_are_collected() {
    with_big_stack(|| {
        let mut ctx = ctx();
        gc::collect();
        let base = gc::live_heap_objects();
        // Each call creates a local scope holding a closure that captures it: a cycle
        ctx.run(
            "test",
            "def loop n = if n == 0 then 0 else { g = x => x; loop (n - 1) }; loop 5000",
        )
        .unwrap();
        gc::collect();
        // `loop` itself and its scope remain
        assert!(
            gc::live_heap_objects() < base + 20,
            "{} live objects after collection",
            gc::live_heap_objects() - base
        );
    });
}
