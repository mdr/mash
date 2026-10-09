//! Rough timings for the value heap and cycle collector: `cargo run --release -p mash-core --example gc_bench`

use std::rc::Rc;
use std::time::{Duration, Instant};

use mash_core::eval::{Ctx, with_big_stack};
use mash_core::platform::TestPlatform;
use mash_core::value::gc::{self, Policy};
use mash_core::value::{List, Object, Scope, ScopeKind, Value};

fn time<T>(f: impl FnOnce() -> T) -> (T, Duration) {
    let start = Instant::now();
    let result = f();
    (result, start.elapsed())
}

/// A local scope holding a value that refers back to it: one garbage cycle per call
fn closure_loop(global: &Scope, n: usize) -> Duration {
    let (_, t) = time(|| {
        for _ in 0..n {
            let local = global.child(ScopeKind::Full, Object::new(), vec![]);
            let holder = Object::new();
            holder.set_str("scope_vars", Value::Object(local.vars().clone()));
            local.vars().set_str("g", Value::Object(holder));
        }
    });
    t
}

fn main() {
    with_big_stack(|| {
        let n = 1_000_000;
        let xs = List::new(vec![Value::num(1.0)]);
        let (_, t) = time(|| (0..n).for_each(|_| drop(std::hint::black_box(xs.clone()))));
        println!("clone+drop: {:.1} ns", t.as_nanos() as f64 / n as f64);
        let (_, t) = time(|| (0..n).for_each(|_| drop(Object::new())));
        println!("alloc+drop (object): {:.1} ns", t.as_nanos() as f64 / n as f64);

        gc::set_policy(Policy::Manual);
        for _ in 0..n {
            let o = Object::new();
            o.set_str("self", Value::Object(o.clone()));
        }
        let (_, t) = time(gc::collect);
        println!("reclaim 1M garbage cycles: {t:?}");

        let live = List::new((0..n).map(|_| Value::Object(Object::new())).collect());
        let (_, t) = time(gc::collect);
        println!("full collection, 1M live objects: {t:?}");

        gc::set_policy(Policy::HeapGrowth);
        let global = Scope::global(Object::new());
        global.vars().set_str("data", Value::List(live));
        let calls = 1_000_000;
        let t = closure_loop(&global, calls);
        println!(
            "closure loop with 1M objects in globals: {:.0} ns/call; collections so far {:?}",
            t.as_nanos() as f64 / calls as f64,
            gc::stats()
        );
        // Worst pause: time each batch of calls that crosses a collection
        let mut worst = Duration::ZERO;
        for _ in 0..2000 {
            worst = worst.max(closure_loop(&global, 1000));
        }
        println!("worst 1000-call batch (includes any pause): {worst:?}");
        drop(global);

        let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
        ctx.run("bench", "def fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)")
            .unwrap();
        let (_, t) = time(|| ctx.run("bench", "fib 25").unwrap());
        println!("fib 25: {t:?}");
    });
}
