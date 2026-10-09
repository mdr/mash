//! Containers never hand out borrows across calls back into Mash, so user code that mutates a list while it is
//! being processed is safe; and collections triggered while a container is being updated are safe.

use mash_core::value::gc::{self, live_heap_objects};
use mash_core::value::{List, Object, Value};

/// Stand-in for calling a user lambda that mutates the list being iterated, as in `xs | map (x => xs.push x)`.
fn user_callback(xs: &List, x: &Value) -> Value {
    xs.push(x.clone());
    x.clone()
}

#[test]
fn snapshot_discipline_makes_mutation_during_iteration_safe() {
    let xs = List::new(vec![Value::num(1.0), Value::num(2.0)]);
    let mapped: Vec<Value> = xs.snapshot().iter().map(|x| user_callback(&xs, x)).collect();
    assert_eq!(mapped.len(), 2);
    assert_eq!(xs.len(), 4); // mutations during the map are visible afterwards, as in Scala
}

#[test]
fn collection_triggered_while_overwriting_a_slot_is_safe() {
    // Overwrite list slots and object fields whose old values are the last references to cycles, collecting
    // each time. The old value is dropped after the borrow is released, so the collector can always trace the
    // container.
    let base = live_heap_objects();
    let xs = List::new(Vec::new());
    let obj = Object::new();
    for i in 0..100 {
        let cyc = Object::new();
        cyc.set_str("self", Value::Object(cyc.clone()));
        xs.push(Value::Object(cyc.clone()));
        obj.set_str("field", Value::Object(cyc));
        xs.set(i, Value::Null);
        gc::collect();
    }
    obj.set_str("field", Value::Null);
    gc::collect();
    assert_eq!(live_heap_objects(), base + 2);
}

#[test]
fn cloning_a_large_list_is_cheap() {
    // Dropping a shared handle only decrements a count (dumpster, the collector used before, scanned the value)
    let xs = List::new((0..1_000_000).map(|i| Value::num(i as f64)).collect());
    let start = std::time::Instant::now();
    for _ in 0..1000 {
        drop(xs.clone());
    }
    assert!(start.elapsed().as_millis() < 500, "took {:?}", start.elapsed());
}
