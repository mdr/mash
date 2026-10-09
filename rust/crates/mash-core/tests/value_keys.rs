//! Values as object keys: hashing and equality, and mutable keys. Also scope assignment rules and ordering.

use std::cmp::Ordering;

use jiff::tz::TimeZone;
use mash_core::value::{BuiltinClass, List, Object, Scope, ScopeKind, Str, Value, compare};

#[test]
fn numeric_key_normalisation() {
    let obj = Object::new();
    obj.set(Value::num(0.0), Value::str("zero"));
    assert!(obj.get(&Value::num(-0.0)).is_some(), "-0.0 and 0.0 are the same key");
    obj.set(Value::num(f64::NAN), Value::str("nan"));
    assert!(obj.get(&Value::num(f64::NAN)).is_some(), "NaN keys are retrievable");
    assert_eq!(obj.len(), 2);
    // ...but the == operator keeps IEEE semantics
    assert!(!Value::num(f64::NAN).mash_equals(&Value::num(f64::NAN)));
    assert!(Value::num(0.0).mash_equals(&Value::num(-0.0)));
}

#[test]
fn structural_keys() {
    let obj = Object::new();
    obj.set(Value::list(vec![Value::num(1.0), Value::str("a")]), Value::Bool(true));
    assert!(obj.get(&Value::list(vec![Value::num(1.0), Value::str("a")])).is_some());
    let k = Object::from_pairs([("x", Value::num(1.0)), ("y", Value::num(2.0))]);
    obj.set(Value::Object(k), Value::Bool(true));
    // Field order doesn't matter for equality or hashing
    let k2 = Object::from_pairs([("y", Value::num(2.0)), ("x", Value::num(1.0))]);
    assert!(obj.get(&Value::Object(k2)).is_some());
}

static PATH: BuiltinClass = BuiltinClass {
    name: "Path",
    namespace: "os",
    parent: None,
};

#[test]
fn tags_are_ignored_by_equality() {
    let obj = Object::new();
    obj.set(Value::Str(Str::tagged("foo", &PATH)), Value::num(1.0));
    assert!(obj.get_str("foo").is_some());
    assert!(obj.get(&Value::str("foo")).is_some());
    assert!(Value::Number(1.0.into(), Some(&PATH)).mash_equals(&Value::num(1.0)));
}

#[test]
fn string_keys_set_by_either_api_are_the_same_key() {
    let obj = Object::new();
    obj.set_str("a", Value::num(1.0));
    obj.set(Value::str("a"), Value::num(2.0));
    obj.set_str("a", Value::num(3.0));
    assert_eq!(obj.len(), 1);
    assert!(obj.get(&Value::str("a")).unwrap().mash_equals(&Value::num(3.0)));
}

#[test]
fn mutating_an_unfrozen_key_breaks_lookup() {
    // What Scala Mash does today (a LinkedHashMap with a mutable key): after mutation the entry can't be found
    // by its old contents, and only sometimes by its new contents. Memory-safe in Rust, but useless.
    let obj = Object::new();
    let keys: Vec<List> = (0..200).map(|i| List::new(vec![Value::num(i as f64)])).collect();
    for k in &keys {
        obj.__set_unfrozen(Value::List(k.clone()), Value::str("v"));
    }
    for k in &keys {
        k.push(Value::num(-1.0));
    }
    let by_old = (0..200)
        .filter(|&i| obj.get(&Value::list(vec![Value::num(i as f64)])).is_some())
        .count();
    let by_new = (0..200)
        .filter(|&i| {
            obj.get(&Value::list(vec![Value::num(i as f64), Value::num(-1.0)]))
                .is_some()
        })
        .count();
    assert_eq!(by_old, 0);
    assert!(by_new < 200);
}

#[test]
fn frozen_keys_are_immune_to_later_mutation() {
    let k = List::new(vec![Value::num(1.0)]);
    let obj = Object::new();
    obj.set(Value::List(k.clone()), Value::str("v"));
    k.push(Value::num(2.0));
    assert!(obj.get(&Value::list(vec![Value::num(1.0)])).is_some());
    // The stored key is a copy, not the caller's list
    let Value::List(stored) = &obj.keys()[0] else { panic!() };
    assert!(!stored.ptr_eq(&k));
}

#[test]
fn cyclic_key_is_frozen_and_hashable() {
    let k = Object::new();
    k.set_str("self", Value::Object(k.clone()));
    let obj = Object::new();
    obj.set(Value::Object(k.clone()), Value::num(1.0));
    assert!(obj.get(&Value::Object(k)).is_some());
}

#[test]
fn scope_assignment_semantics() {
    let global = Scope::global(Object::new());
    let lambda = global.child(ScopeKind::Leaky, Object::new(), vec![]);
    lambda.assign("a", Value::num(1.0)); // leaks to the global scope
    assert!(global.lookup("a").is_some());
    let def = global.child(ScopeKind::Full, Object::new(), vec![]);
    def.assign("b", Value::num(2.0)); // stays local
    assert!(global.lookup("b").is_none());
    assert!(def.lookup("b").is_some());
    def.assign("a", Value::num(3.0)); // a full scope binds locally even if an outer `a` exists
    assert!(global.lookup("a").unwrap().mash_equals(&Value::num(1.0)));
    lambda.assign("a", Value::num(4.0)); // an existing outer variable is updated through a leaky scope
    assert!(global.lookup("a").unwrap().mash_equals(&Value::num(4.0)));
}

#[test]
fn assigning_to_a_field_of_this() {
    let global = Scope::global(Object::new());
    let this = Object::from_pairs([("x", Value::num(1.0))]);
    let method = global.child_with_this(Object::new(), vec![], Value::Object(this.clone()));
    method.assign("x", Value::num(2.0));
    assert!(this.get_str("x").unwrap().mash_equals(&Value::num(2.0)));
    assert!(method.vars().is_empty());
    let binding = method.lookup_with("x", |t, name| match t {
        Value::Object(o) => o.get_str(name),
        _ => None,
    });
    assert!(binding.unwrap().value.mash_equals(&Value::num(2.0)));
}

#[test]
fn ordering() {
    let tz = TimeZone::UTC;
    let sorted = [
        Value::Null,
        Value::Unit,
        Value::Bool(false),
        Value::Bool(true),
        Value::num(-1.0),
        Value::num(2.0),
        Value::num(f64::NAN),
        Value::str("B"),
        Value::str("a"),
        Value::list(vec![Value::num(1.0)]),
        Value::list(vec![Value::num(1.0), Value::num(0.0)]),
        Value::list(vec![Value::num(2.0)]),
        Value::Object(Object::from_pairs([("a", Value::num(1.0))])),
        Value::Object(Object::from_pairs([("a", Value::num(2.0))])),
        Value::Object(Object::from_pairs([("b", Value::num(0.0))])),
    ];
    for (i, a) in sorted.iter().enumerate() {
        for (j, b) in sorted.iter().enumerate() {
            assert_eq!(compare(a, b, &tz), i.cmp(&j), "comparing {a:?} with {b:?}");
        }
    }
}

#[test]
fn ordering_terminates_on_cycles() {
    let xs = List::new(vec![]);
    xs.push(Value::List(xs.clone()));
    let ys = List::new(vec![]);
    ys.push(Value::List(ys.clone()));
    assert_eq!(
        compare(&Value::List(xs), &Value::List(ys), &TimeZone::UTC),
        Ordering::Equal
    );
}

#[test]
fn dates_compare_with_date_times_at_midnight() {
    use jiff::civil::date;
    use mash_core::value::{DateTime, LocalDate};
    let tz = TimeZone::fixed(jiff::tz::offset(2));
    let day = Value::Date(LocalDate(date(2026, 10, 9)));
    // 2026-10-09T00:00+02:00 is 2026-10-08T22:00Z
    let before = Value::DateTime(DateTime("2026-10-08T21:59:59Z".parse().unwrap()));
    let at = Value::DateTime(DateTime("2026-10-08T22:00:00Z".parse().unwrap()));
    assert_eq!(compare(&day, &before, &tz), Ordering::Greater);
    assert_eq!(compare(&day, &at, &tz), Ordering::Equal);
    assert_eq!(compare(&at, &day, &tz), Ordering::Equal);
}
