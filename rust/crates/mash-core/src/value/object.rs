use std::cell::RefCell;
use std::hash::{Hash, Hasher};

use indexmap::{Equivalent, IndexMap};

use super::gc::{Gc, Trace, Tracer};
use super::{ClassRef, Str, Value, hash_str_key};

/// A mutable, shared, insertion-ordered Mash object. Keys may be any value; an object may have a class.
///
/// Like [`List`](super::List), no borrow escapes: iterate via [`Object::entries`].
#[derive(Clone)]
pub struct Object(Gc<ObjectData>);

struct ObjectData {
    fields: RefCell<IndexMap<Value, Value>>,
    class: Option<ClassRef>,
}

// SAFETY: visits every key (frozen list and object keys hold `Gc`s too), value and the class.
#[allow(unsafe_code)]
unsafe impl Trace for ObjectData {
    fn trace(&self, tracer: &mut Tracer) {
        match self.fields.try_borrow() {
            Ok(fields) => {
                for (k, v) in fields.iter() {
                    k.trace(tracer);
                    v.trace(tracer);
                }
            }
            Err(_) => tracer.fail(),
        }
        self.class.trace(tracer);
    }
}

// SAFETY: visits its one handle.
#[allow(unsafe_code)]
unsafe impl Trace for Object {
    fn trace(&self, tracer: &mut Tracer) {
        tracer.edge(&self.0);
    }
}

/// Looks up a string key without allocating a `Value`.
struct StrKey<'a>(&'a str);

impl Hash for StrKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_str_key(self.0, state);
    }
}

impl Equivalent<Value> for StrKey<'_> {
    fn equivalent(&self, key: &Value) -> bool {
        matches!(key, Value::Str(s) if s.as_str() == self.0)
    }
}

impl Object {
    pub fn new() -> Self {
        Object::with_class(None)
    }

    pub fn with_class(class: Option<ClassRef>) -> Self {
        Object(Gc::new(ObjectData {
            fields: RefCell::new(IndexMap::new()),
            class,
        }))
    }

    pub fn from_pairs<K: Into<Value>>(pairs: impl IntoIterator<Item = (K, Value)>) -> Self {
        let obj = Object::new();
        for (k, v) in pairs {
            obj.set(k.into(), v);
        }
        obj
    }

    pub fn class(&self) -> Option<ClassRef> {
        self.0.class.clone()
    }

    pub fn len(&self) -> usize {
        self.0.fields.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, key: &Value) -> Option<Value> {
        self.0.fields.borrow().get(key).cloned()
    }

    pub fn get_str(&self, key: &str) -> Option<Value> {
        self.0.fields.borrow().get(&StrKey(key)).cloned()
    }

    pub fn contains(&self, key: &Value) -> bool {
        self.0.fields.borrow().contains_key(key)
    }

    pub fn contains_str(&self, key: &str) -> bool {
        self.0.fields.borrow().contains_key(&StrKey(key))
    }

    /// Insert or replace a field (keeping its position if it exists). List and object keys are deep-copied
    /// ("frozen"), so later mutation of the caller's container can't corrupt the map.
    pub fn set(&self, key: Value, value: Value) {
        let key = key.freeze_as_key();
        let old = self.0.fields.borrow_mut().insert(key, value);
        // Dropped after the borrow is released, so that nothing it frees can find this cell borrowed
        drop(old);
    }

    pub fn set_str(&self, key: &str, value: Value) {
        let old = {
            let mut fields = self.0.fields.borrow_mut();
            match fields.get_mut(&StrKey(key)) {
                Some(slot) => Some(std::mem::replace(slot, value)),
                None => fields.insert(Value::Str(Str::new(key)), value),
            }
        };
        drop(old);
    }

    /// Remove a field, keeping the order of the others
    pub fn remove(&self, key: &Value) -> Option<Value> {
        self.0.fields.borrow_mut().shift_remove(key)
    }

    pub fn keys(&self) -> Vec<Value> {
        self.0.fields.borrow().keys().cloned().collect()
    }

    pub fn values(&self) -> Vec<Value> {
        self.0.fields.borrow().values().cloned().collect()
    }

    /// A copy of the fields, in order. Use this to iterate.
    pub fn entries(&self) -> Vec<(Value, Value)> {
        self.0
            .fields
            .borrow()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    /// A shallow copy, optionally with a different class
    pub fn copy_with_class(&self, class: Option<ClassRef>) -> Object {
        let copy = Object::with_class(class);
        // Keys are already frozen
        *copy.0.fields.borrow_mut() = {
            let mut map = IndexMap::new();
            map.extend(self.entries());
            map
        };
        copy
    }

    /// Run `f` on the fields, which must not call back into the interpreter (or anything else that could
    /// touch this object). For cheap read-only operations that shouldn't pay for a copy.
    pub fn with_fields<T>(&self, f: impl FnOnce(&indexmap::IndexMap<Value, Value>) -> T) -> T {
        f(&self.0.fields.borrow())
    }

    pub fn ptr_eq(&self, other: &Object) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn addr(&self) -> usize {
        self.0.addr()
    }

    /// Insert without freezing the key, to demonstrate why keys are frozen
    #[doc(hidden)]
    pub fn __set_unfrozen(&self, key: Value, value: Value) {
        let old = self.0.fields.borrow_mut().insert(key, value);
        drop(old);
    }
}

impl Default for Object {
    fn default() -> Self {
        Object::new()
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&Value::Object(self.clone()), f)
    }
}
