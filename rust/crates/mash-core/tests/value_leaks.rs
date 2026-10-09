//! Every cyclic shape Mash can create must be reclaimed.
//!
//! Each test runs on its own thread (cargo's default), and both the live-object count and the collector are
//! thread-local, so tests don't interfere.

use std::rc::Rc;

use mash_core::ast::{Expr, ExprKind, FnDef, NodeIdGen, SourceUnit};
use mash_core::function::{Params, Suspended};
use mash_core::value::gc::{self, Policy, live_heap_objects};
use mash_core::value::{Class, ClassRef, Function, List, Object, Scope, ScopeKind, UserMethod, Value};

fn def(name: &str) -> Rc<FnDef> {
    let mut ids = NodeIdGen::default();
    Rc::new(FnDef {
        name: Some(name.into()),
        doc: None,
        attributes: vec![],
        params: vec![],
        body: Expr::new(&mut ids, None, ExprKind::Missing),
        namespace: None,
        unit: SourceUnit::new("test", ""),
        range: None,
    })
}

fn closure(name: &str, scope: &Scope) -> Function {
    Function::closure(def(name), Rc::new(Params::default()), scope.clone())
}

fn local_scope(parent: &Scope) -> Scope {
    parent.child(ScopeKind::Full, Object::new(), vec![])
}

/// Collect, and check nothing survived.
#[track_caller]
fn assert_all_freed(baseline: usize) {
    gc::collect();
    assert_eq!(live_heap_objects(), baseline, "heap objects leaked");
}

#[test]
fn a_global_scope_holding_a_closure_that_captures_it() {
    let base = live_heap_objects();
    {
        let global = Scope::global(Object::new());
        let f = closure("f", &global);
        global.vars().set_str("f", Value::Function(f));
        assert_eq!(live_heap_objects(), base + 3); // scope, its variables object, closure
    }
    assert_all_freed(base);
}

#[test]
fn b_local_closures_in_a_loop_stay_bounded_without_explicit_collection() {
    let base = live_heap_objects();
    let global = Scope::global(Object::new());
    // Some long-lived global data, so collections have something to (not) traverse
    global
        .vars()
        .set_str("data", Value::list((0..1000).map(|i| Value::num(i as f64)).collect()));

    let mut max_live = 0;
    for _ in 0..100_000 {
        // def foo = { g = x => x + 1; g 1 }: the local scope holds g, and g captures the scope
        let local = local_scope(&global);
        let g = closure("g", &local);
        local.vars().set_str("g", Value::Function(g));
        max_live = max_live.max(live_heap_objects());
    }
    // 100k iterations create 300k objects, so automatic collection must have kicked in
    assert!(
        max_live < base + 30_000,
        "garbage is not being collected: max live {max_live}"
    );
    assert!(gc::growth_collections() > 0);

    drop(global);
    assert_all_freed(base);
}

#[test]
fn b2_manual_policy_only_collects_when_asked() {
    gc::set_policy(Policy::Manual);
    let base = live_heap_objects();
    let global = Scope::global(Object::new());
    for _ in 0..20_000 {
        let local = local_scope(&global);
        local.vars().set_str("g", Value::Function(closure("g", &local)));
    }
    assert!(live_heap_objects() >= base + 60_000);
    drop(global);
    assert_all_freed(base);
}

#[test]
fn c_object_containing_itself() {
    let base = live_heap_objects();
    {
        let a = Object::new();
        a.set_str("self", Value::Object(a.clone()));
        assert!(matches!(a.get_str("self"), Some(Value::Object(o)) if o.ptr_eq(&a)));
        // Debug printing and equality terminate on cycles
        assert_eq!(format!("{:?}", Value::Object(a.clone())), "{\"self\": {…}}");
        assert!(Value::Object(a.clone()).mash_equals(&Value::Object(a.clone())));
    }
    assert_all_freed(base);
}

#[test]
fn d_list_containing_itself() {
    let base = live_heap_objects();
    {
        let xs = List::new(vec![Value::num(1.0)]);
        xs.push(Value::List(xs.clone()));
        assert_eq!(format!("{:?}", Value::List(xs.clone())), "[1, […]]");
    }
    assert_all_freed(base);
}

#[test]
fn e_mutually_recursive_closures() {
    let base = live_heap_objects();
    {
        let global = Scope::global(Object::new());
        let local = local_scope(&global);
        local
            .vars()
            .set_str("isEven", Value::Function(closure("isEven", &local)));
        local.vars().set_str("isOdd", Value::Function(closure("isOdd", &local)));
    }
    assert_all_freed(base);
}

#[test]
fn f_object_class_method_scope_object() {
    let base = live_heap_objects();
    {
        // class Box n { def get = n }; b = Box 42, declared in the global scope, which holds both
        let global = Scope::global(Object::new());
        let class_def = Rc::new(mash_core::ast::ClassDef {
            name: "Box".into(),
            doc: None,
            attributes: vec![],
            params: vec![],
            methods: vec![def("get")],
            namespace: None,
            unit: SourceUnit::new("test", ""),
            range: None,
        });
        let method = UserMethod {
            def: def("get"),
            params: Rc::new(Params::default()),
            aliases: vec![],
            private: false,
        };
        let class = Class::new(class_def, Rc::new(Params::default()), vec![method], global.clone());
        global
            .vars()
            .set_str("Box", Value::Class(ClassRef::User(class.clone())));
        let instance = Object::with_class(Some(ClassRef::User(class)));
        instance.set_str("n", Value::num(42.0));
        global.vars().set_str("b", Value::Object(instance.clone()));
        // A method call's scope, with `this` bound, kept alive by a closure stored in `this`
        let call_scope = global.child_with_this(Object::new(), vec![], Value::Object(instance.clone()));
        instance.set_str("callback", Value::Function(closure("cb", &call_scope)));
    }
    assert_all_freed(base);
}

#[test]
fn mixed_reachable_and_unreachable_cycles() {
    let base = live_heap_objects();
    let keep = Object::new();
    keep.set_str("self", Value::Object(keep.clone()));
    {
        let garbage = Object::new();
        garbage.set_str("self", Value::Object(garbage.clone()));
        garbage.set_str("keep", Value::Object(keep.clone()));
    }
    gc::collect();
    assert_eq!(
        live_heap_objects(),
        base + 1,
        "the reachable cycle must survive and the garbage must go"
    );
    assert!(keep.get_str("self").is_some());
    drop(keep);
    assert_all_freed(base);
}

#[test]
fn bound_methods_and_suspended_values_are_traced() {
    use mash_core::value::{BoundMethod, FunctionKind, MethodRef};
    let base = live_heap_objects();
    {
        let global = Scope::global(Object::new());
        let mut ids = NodeIdGen::default();
        // A lazy argument thunk capturing the scope that holds it
        let thunk = Function::new(FunctionKind::Suspended(Suspended::Expr {
            expr: Expr::new(&mut ids, None, ExprKind::Missing),
            scope: global.clone(),
            unit: SourceUnit::new("test", ""),
        }));
        global.vars().set_str("thunk", Value::Function(thunk));
        // A suspended value holding the object it's stored in
        let holder = Object::new();
        let value_thunk = Function::new(FunctionKind::Suspended(Suspended::Value(Value::Object(holder.clone()))));
        holder.set_str("thunk", Value::Function(value_thunk));
        // A user method bound to an object that holds the bound method
        let class_def = Rc::new(mash_core::ast::ClassDef {
            name: "C".into(),
            doc: None,
            attributes: vec![],
            params: vec![],
            methods: vec![],
            namespace: None,
            unit: SourceUnit::new("test", ""),
            range: None,
        });
        let method = UserMethod {
            def: def("m"),
            params: Rc::new(Params::default()),
            aliases: vec![],
            private: false,
        };
        let class = Class::new(class_def, Rc::new(Params::default()), vec![method], global.clone());
        let obj = Object::with_class(Some(ClassRef::User(class.clone())));
        let bound = BoundMethod::new(Value::Object(obj.clone()), MethodRef::User { class, index: 0 });
        obj.set_str("m", Value::BoundMethod(bound));
        global.vars().set_str("obj", Value::Object(obj));
    }
    assert_all_freed(base);
}
