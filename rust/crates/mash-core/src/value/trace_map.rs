//! `IndexMap` doesn't implement dumpster's `Trace`, so wrap it.

use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use dumpster::{TraceWith, Visitor};
use indexmap::IndexMap;

pub(crate) struct TraceMap<K, V>(IndexMap<K, V>);

impl<K, V> Default for TraceMap<K, V> {
    fn default() -> Self {
        TraceMap(IndexMap::new())
    }
}

impl<K: Hash + Eq, V> Deref for TraceMap<K, V> {
    type Target = IndexMap<K, V>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K: Hash + Eq, V> DerefMut for TraceMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// SAFETY: visits every key and value, in a consistent (insertion) order.
#[allow(unsafe_code)]
unsafe impl<Z: Visitor, K: TraceWith<Z>, V: TraceWith<Z>> TraceWith<Z> for TraceMap<K, V> {
    fn accept(&self, visitor: &mut Z) -> Result<(), ()> {
        for (k, v) in &self.0 {
            k.accept(visitor)?;
            v.accept(visitor)?;
        }
        Ok(())
    }
}
