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
        let error = eval_error(&mut ctx, "x = 42; x.toint");
        assert!(
            error
                .message
                .starts_with("Cannot find member 'toint' in value of type Number. Did you mean"),
            "{}",
            error.message
        );
        assert_eq!(error.location().map(|l| l.snippet()), Some("toint"));
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

/// A platform whose user asks to interrupt evaluation the `nth` time it's asked (as Ctrl-C in the browser)
#[derive(Debug)]
struct InterruptingPlatform {
    inner: TestPlatform,
    polls: std::cell::Cell<u32>,
    nth: u32,
}

impl mash_core::platform::Platform for InterruptingPlatform {
    fn env_var(&self, name: &str) -> Option<String> {
        self.inner.env_var(name)
    }
    fn env_vars(&self) -> Vec<(String, String)> {
        self.inner.env_vars()
    }
    fn current_dir(&self) -> Option<String> {
        self.inner.current_dir()
    }
    fn home_dir(&self) -> Option<String> {
        self.inner.home_dir()
    }
    fn time_zone(&self) -> jiff::tz::TimeZone {
        self.inner.time_zone()
    }
    fn now(&self) -> jiff::Timestamp {
        self.inner.now()
    }
    fn read_dir(&self, path: &str) -> std::io::Result<Vec<mash_core::platform::DirEntry>> {
        self.inner.read_dir(path)
    }
    fn sleep(&self, duration: std::time::Duration) {
        self.inner.sleep(duration)
    }
    fn random_u64(&self) -> u64 {
        self.inner.random_u64()
    }
    fn write_output(&self, text: &str) {
        self.inner.write_output(text)
    }
    fn interrupt_requested(&self) -> bool {
        self.polls.set(self.polls.get() + 1);
        self.polls.get() == self.nth
    }
}

#[test]
fn the_platform_can_interrupt_evaluation() {
    with_big_stack(|| {
        let platform = Rc::new(InterruptingPlatform {
            inner: TestPlatform::default(),
            polls: Default::default(),
            nth: 3,
        });
        let mut ctx = Ctx::new(platform.clone());
        let error = eval_error(&mut ctx, "1.to 1000000 | map (n => n + 1) | sum");
        assert!(error.is_interrupted(), "{}", error.message);
        assert_eq!(platform.polls.get(), 3);
        // Asked while sleeping, too
        platform.polls.set(0);
        let error = eval_error(&mut ctx, "time.sleep 10.seconds");
        assert!(error.is_interrupted(), "{}", error.message);
        // A new run starts uninterrupted
        assert_eq!(format!("{:?}", ctx.run("test", "1 + 1").unwrap()), "2");
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

#[test]
fn error_reports_show_the_source_and_the_call_stack() {
    with_big_stack(|| {
        let mut ctx = ctx();
        let source = "def inner = error 'boom'\ndef outer = inner\nouter";
        let report = eval_error(&mut ctx, source).report();
        assert_eq!(
            report,
            "error: boom\n  --> test:1:13\n   | def inner = error 'boom'\n   |             ^^^^^^^^^^^^\n  called at test:2:13 in outer\n  called at test:3:1"
        );
    });
}

#[test]
fn library_files_run_in_their_own_scope_and_report_their_namespace() {
    with_big_stack(|| {
        let mut ctx = ctx();
        let (namespace, vars) = ctx
            .load_library(
                "lib/greet.mash",
                "namespace acme.greetings\ndef hello name = \"Hello, \" + name",
            )
            .unwrap();
        assert_eq!(namespace, Some(vec!["acme".to_string(), "greetings".to_string()]));
        assert!(vars.get_str("hello").is_some());
        // Nothing leaked into the globals
        assert!(ctx.run("test", "hello 'x'").is_err());
    });
}
