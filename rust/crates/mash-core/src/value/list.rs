use std::cell::{Cell, RefCell};

use super::Value;
use super::gc::{Gc, Trace, Tracer};

/// A mutable, shared Mash list.
///
/// No method lets a borrow of the contents escape, so it is always safe to call back into the interpreter
/// (which may mutate this same list) between calls. To iterate, take a [`List::snapshot`]: cloning `Value`s
/// is cheap.
#[derive(Clone)]
pub struct List(Gc<ListData>);

struct ListData {
    items: RefCell<Vec<Value>>,
    /// How many elements hold `Gc`s. When zero (lists of numbers or strings) tracing is skipped entirely.
    /// Kept exact by every mutation below.
    heap_items: Cell<usize>,
}

// SAFETY: visits every element (skipping the scan when none holds a `Gc`: `heap_items` is exact).
#[allow(unsafe_code)]
unsafe impl Trace for ListData {
    fn trace(&self, tracer: &mut Tracer) {
        if self.heap_items.get() == 0 {
            return;
        }
        match self.items.try_borrow() {
            Ok(items) => items.iter().for_each(|item| item.trace(tracer)),
            Err(_) => tracer.fail(),
        }
    }
}

// SAFETY: visits its one handle.
#[allow(unsafe_code)]
unsafe impl Trace for List {
    fn trace(&self, tracer: &mut Tracer) {
        tracer.edge(&self.0);
    }
}

fn count_heap(items: &[Value]) -> usize {
    items.iter().filter(|v| v.holds_gc()).count()
}

impl List {
    pub fn new(items: Vec<Value>) -> Self {
        let heap_items = Cell::new(count_heap(&items));
        List(Gc::new(ListData {
            items: RefCell::new(items),
            heap_items,
        }))
    }

    pub fn len(&self) -> usize {
        self.0.items.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Resolve a Mash index (negative counts from the end), if in range.
    pub fn resolve_index(&self, index: i64) -> Option<usize> {
        let len = self.len() as i64;
        let i = if index < 0 { index + len } else { index };
        (0..len).contains(&i).then_some(i as usize)
    }

    /// Element at `index`; negative indexes count from the end.
    pub fn get(&self, index: i64) -> Option<Value> {
        let i = self.resolve_index(index)?;
        self.0.items.borrow().get(i).cloned()
    }

    pub fn first(&self) -> Option<Value> {
        self.0.items.borrow().first().cloned()
    }

    pub fn last(&self) -> Option<Value> {
        self.0.items.borrow().last().cloned()
    }

    /// Replace the element at `index` (negative counts from the end). Returns false if out of range.
    pub fn set(&self, index: i64, value: Value) -> bool {
        let Some(i) = self.resolve_index(index) else {
            return false;
        };
        let new_heap = usize::from(value.holds_gc());
        let old = std::mem::replace(&mut self.0.items.borrow_mut()[i], value);
        self.0
            .heap_items
            .set(self.0.heap_items.get() + new_heap - usize::from(old.holds_gc()));
        // Dropped after the borrow is released, so that nothing it frees can find this cell borrowed
        drop(old);
        true
    }

    pub fn push(&self, value: Value) {
        if value.holds_gc() {
            self.0.heap_items.set(self.0.heap_items.get() + 1);
        }
        self.0.items.borrow_mut().push(value);
    }

    pub fn pop(&self) -> Option<Value> {
        let v = self.0.items.borrow_mut().pop();
        if v.as_ref().is_some_and(Value::holds_gc) {
            self.0.heap_items.set(self.0.heap_items.get() - 1);
        }
        v
    }

    /// Replace the whole contents (e.g. after sorting a snapshot).
    pub fn replace_all(&self, items: Vec<Value>) {
        let n = count_heap(&items);
        let old = std::mem::replace(&mut *self.0.items.borrow_mut(), items);
        self.0.heap_items.set(n);
        drop(old);
    }

    /// A copy of the current elements. Use this to iterate.
    pub fn snapshot(&self) -> Vec<Value> {
        self.0.items.borrow().clone()
    }

    /// Run `f` on the elements, which must not call back into the interpreter (or anything else that could
    /// touch this list). For cheap read-only operations that shouldn't pay for a snapshot.
    pub fn with_items<T>(&self, f: impl FnOnce(&[Value]) -> T) -> T {
        f(&self.0.items.borrow())
    }

    pub fn ptr_eq(&self, other: &List) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn addr(&self) -> usize {
        self.0.addr()
    }
}

impl FromIterator<Value> for List {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        List::new(iter.into_iter().collect())
    }
}

impl std::fmt::Debug for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&Value::List(self.clone()), f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::{Object, gc};

    #[test]
    fn the_contents_of_a_list_being_mutated_during_a_collection_survive() {
        let base = gc::live_heap_objects();
        let xs = List::new(vec![]);
        // A cycle referenced only from the list
        let o = Object::new();
        o.set_str("self", Value::Object(o.clone()));
        xs.push(Value::Object(o));
        {
            let _borrowed = xs.0.items.borrow_mut();
            gc::collect();
            assert_eq!(gc::live_heap_objects(), base + 2);
        }
        let Some(Value::Object(o)) = xs.get(0) else {
            panic!("expected the object");
        };
        assert!(o.get_str("self").is_some());
        drop((o, xs));
        gc::collect();
        assert_eq!(gc::live_heap_objects(), base);
    }
}
