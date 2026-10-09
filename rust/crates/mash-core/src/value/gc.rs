//! Garbage collection of cyclic values: live-object accounting and the collection policy.
//!
//! Values are reference counted, with `dumpster`'s cycle collector reclaiming unreachable cycles (closures
//! capture scopes, and scopes are objects, so cycles are routine). This module is the only place that knows
//! about the collector's policy; everything else just uses the value types.
//!
//! dumpster's own trigger ("more handles dropped since the last collection than currently exist") never
//! fires for the closure-in-a-loop pattern, where each call creates several handles but drops only one or
//! two. So we install our own: collect when the number of live heap values has doubled since the last
//! collection. The REPL should also call [`collect`] whenever it is idle.

use std::cell::Cell;

use dumpster::unsync::{CollectInfo, set_collect_condition};
use dumpster::{TraceWith, Visitor};

/// Below this many live heap values we never collect automatically.
const MIN_THRESHOLD: usize = 10_000;

thread_local! {
    static LIVE: Cell<usize> = const { Cell::new(0) };
    static THRESHOLD: Cell<usize> = const { Cell::new(MIN_THRESHOLD) };
    /// Set when we've asked for a collection: the threshold is recomputed from the surviving heap at the next
    /// check (dumpster has no post-collection hook).
    static RECOMPUTE: Cell<bool> = const { Cell::new(false) };
    static POLICY: Cell<Option<Policy>> = const { Cell::new(None) };
    static GROWTH_COLLECTIONS: Cell<usize> = const { Cell::new(0) };
}

/// When cycles are collected automatically (on this thread).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Policy {
    /// Only when [`collect`] is called.
    Manual,
    /// When the live heap has doubled since the last collection (the default).
    HeapGrowth,
}

/// Choose the automatic collection policy for this thread.
pub fn set_policy(policy: Policy) {
    POLICY.with(|p| p.set(Some(policy)));
    match policy {
        Policy::Manual => set_collect_condition(|_| false),
        Policy::HeapGrowth => set_collect_condition(heap_growth_condition),
    }
}

/// Collect unreachable cycles now, and reset the growth threshold from the surviving heap.
pub fn collect() {
    dumpster::unsync::collect();
    RECOMPUTE.with(|r| r.set(false));
    THRESHOLD.with(|t| t.set(next_threshold(live_heap_objects())));
}

/// Number of heap-allocated values (lists, objects, functions, scopes, classes, bound methods) alive on this
/// thread.
pub fn live_heap_objects() -> usize {
    LIVE.with(Cell::get)
}

/// How many collections the heap-growth trigger has started on this thread.
pub fn growth_collections() -> usize {
    GROWTH_COLLECTIONS.with(Cell::get)
}

fn heap_growth_condition(info: &CollectInfo) -> bool {
    let live = live_heap_objects();
    if RECOMPUTE.with(|r| r.replace(false)) {
        THRESHOLD.with(|t| t.set(next_threshold(live)));
    }
    if live > THRESHOLD.with(Cell::get) {
        RECOMPUTE.with(|r| r.set(true));
        GROWTH_COLLECTIONS.with(|c| c.set(c.get() + 1));
        return true;
    }
    dumpster::unsync::default_collect_condition(info)
}

fn next_threshold(live: usize) -> usize {
    (live * 2).max(MIN_THRESHOLD)
}

/// Embedded in every heap value: counts construction and destruction.
pub(crate) struct Tracked(());

impl Tracked {
    pub(crate) fn new() -> Self {
        if POLICY.with(Cell::get).is_none() {
            set_policy(Policy::HeapGrowth);
        }
        LIVE.with(|l| l.set(l.get() + 1));
        Tracked(())
    }
}

impl Drop for Tracked {
    fn drop(&mut self) {
        LIVE.with(|l| l.set(l.get() - 1));
    }
}

// SAFETY: contains no `Gc`s.
#[allow(unsafe_code)]
unsafe impl<V: Visitor> TraceWith<V> for Tracked {
    fn accept(&self, _: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

/// True when `V` is dumpster's private `ContainsGcs` visitor, which only asks "is there at least one `Gc` in
/// here?", and so can stop at the first. dumpster runs it over the whole value every time a shared handle is
/// dropped, which made cloning a large list linear in its size. A hack (it matches a private type name) that
/// goes away with dumpster.
pub(crate) fn is_contains_gcs_visitor<V>() -> bool {
    std::any::type_name::<V>().ends_with("ContainsGcs")
}
