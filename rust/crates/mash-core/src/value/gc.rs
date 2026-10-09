//! Garbage collection: reference counting plus a cycle collector, after CPython's design.
//!
//! Every heap value (list, object, scope, function, bound method, user class) lives in a [`Gc`] box with a
//! reference count, so acyclic garbage is freed as soon as its last handle goes. Closures capture scopes and
//! scopes are objects, so cycles are routine; those are found by trial deletion over the tracked boxes:
//!
//! 1. For each box in the generation(s) being collected, start `gc_refs` at its reference count.
//! 2. Trace each box, subtracting one from every box it references within the set. What's left counts the
//!    references from outside the set: from Rust locals (the evaluator's stack), or from older boxes.
//! 3. Boxes with outside references are reachable, as is everything reachable from them. The rest are
//!    unreachable cycles: drop their contents, then free them.
//!
//! No roots are registered (a reference held anywhere in Rust is an outside reference), so it is safe to
//! collect in the middle of evaluation. A container that can't be traced right now (its `RefCell` is mutably
//! borrowed) is conservatively treated as reachable.
//!
//! Boxes start in the young generation; survivors of a collection move to the old one. A young collection runs
//! when enough young boxes are alive, and looks only at those, so a loop creating short-lived closures never
//! re-traces a large long-lived heap. A full collection runs instead when the old generation has doubled since
//! the last one, so the total cost stays proportional to allocation. The REPL should also call [`collect`] whenever it
//! is idle.
//!
//! Freeing is iterative: a box whose count reaches zero while another is being freed is queued rather than
//! freed recursively, so dropping a long chain (a list nested a million deep) can't overflow the stack.
//!
//! Everything here is per thread: a value must never be sent to another thread (`Gc` is `!Send`).

#![allow(unsafe_code)]

use std::cell::{Cell, RefCell};
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ops::Deref;
use std::ptr::NonNull;

/// A young collection runs when this many young boxes are alive.
const YOUNG_THRESHOLD: usize = 10_000;
/// Below this many old boxes, young collections never escalate to full ones.
const MIN_FULL_THRESHOLD: usize = 10_000;

/// When cycles are collected automatically (on this thread).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Policy {
    /// Only when [`collect`] is called.
    Manual,
    /// When enough new boxes are alive (the default).
    HeapGrowth,
}

/// Counts of the collections run so far on this thread.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Stats {
    /// Automatic collections of the young generation only
    pub young: usize,
    /// Automatic full collections
    pub full: usize,
    /// Explicit full collections ([`collect`])
    pub explicit: usize,
}

/// Choose the automatic collection policy for this thread.
pub fn set_policy(policy: Policy) {
    HEAP.with(|heap| heap.borrow_mut().policy = policy);
}

/// Collect all unreachable cycles now.
pub fn collect() {
    if run_collection(Generation::Old) {
        HEAP.with(|heap| heap.borrow_mut().stats.explicit += 1);
    }
}

/// Number of heap values (lists, objects, functions, scopes, classes, bound methods) alive on this thread.
pub fn live_heap_objects() -> usize {
    HEAP.try_with(|heap| heap.borrow().live()).unwrap_or(0)
}

/// The collections run so far on this thread.
pub fn stats() -> Stats {
    HEAP.with(|heap| heap.borrow().stats)
}

/// Types that can live in a [`Gc`] box, or be part of one.
///
/// # Safety
///
/// `trace` must report via [`Tracer::edge`] only `Gc`s the value owns (directly, or within non-`Gc` data it
/// owns), each once for each handle held. Reporting one it doesn't own can free a live box. Missing one is
/// safe but leaks. If it can't look right now, it must call [`Tracer::fail`].
pub(crate) unsafe trait Trace {
    fn trace(&self, tracer: &mut Tracer);
}

// SAFETY: visits the value, if any.
unsafe impl<T: Trace> Trace for Option<T> {
    fn trace(&self, tracer: &mut Tracer) {
        if let Some(value) = self {
            value.trace(tracer);
        }
    }
}

/// Collects the edges out of a box.
#[derive(Default)]
pub(crate) struct Tracer {
    edges: Vec<NonNull<Header>>,
    failed: bool,
}

impl Tracer {
    /// The value being traced holds this handle
    pub(crate) fn edge<T: Trace>(&mut self, gc: &Gc<T>) {
        self.edges.push(gc.header());
    }

    /// The value can't be traced right now (it is being mutated)
    pub(crate) fn fail(&mut self) {
        self.failed = true;
    }

    /// Trace one box, returning its edges, or None if it couldn't be traced
    fn trace(&mut self, header: NonNull<Header>) -> Option<&[NonNull<Header>]> {
        self.edges.clear();
        self.failed = false;
        // SAFETY: tracked boxes are alive.
        unsafe { (header.as_ref().vtable.trace)(header, self) };
        (!self.failed).then_some(&self.edges)
    }
}

/// A reference-counted handle to a value that takes part in cycle collection.
pub(crate) struct Gc<T: Trace + 'static> {
    ptr: NonNull<GcBox<T>>,
    // Owns a `GcBox<T>`; not `Send` or `Sync` (also implied by the raw pointer)
    _marker: PhantomData<GcBox<T>>,
}

#[repr(C)]
struct GcBox<T> {
    // First, so that a pointer to the box is a pointer to its header
    header: Header,
    /// Dropped by the vtable's `drop_value`, separately from freeing the box
    value: ManuallyDrop<T>,
}

struct Header {
    strong: Cell<usize>,
    /// Scratch space for a collection: references from outside the set being collected
    gc_refs: Cell<usize>,
    /// Position in its generation's list
    index: Cell<usize>,
    generation: Cell<Generation>,
    flags: Cell<u8>,
    vtable: &'static VTable,
}

/// In the set being collected
const IN_SET: u8 = 1;
/// Reachable from outside the set
const REACHABLE: u8 = 2;
/// Couldn't be traced, so treated as reachable
const UNTRACEABLE: u8 = 4;

struct VTable {
    trace: unsafe fn(NonNull<Header>, &mut Tracer),
    drop_value: unsafe fn(NonNull<Header>),
    dealloc: unsafe fn(NonNull<Header>),
}

impl<T: Trace + 'static> GcBox<T> {
    const VTABLE: VTable = VTable {
        trace: trace_box::<T>,
        drop_value: drop_box_value::<T>,
        dealloc: dealloc_box::<T>,
    };
}

/// # Safety
/// `header` must be a live `GcBox<T>`.
unsafe fn trace_box<T: Trace>(header: NonNull<Header>, tracer: &mut Tracer) {
    // SAFETY: the caller guarantees the box is a live `GcBox<T>`.
    let gc_box = unsafe { header.cast::<GcBox<T>>().as_ref() };
    gc_box.value.trace(tracer);
}

/// # Safety
/// `header` must be a `GcBox<T>` whose value hasn't been dropped and won't be used again.
unsafe fn drop_box_value<T>(header: NonNull<Header>) {
    let gc_box = header.cast::<GcBox<T>>().as_ptr();
    // SAFETY: the caller guarantees no one else uses the value; `ManuallyDrop<T>` has `T`'s layout.
    unsafe { std::ptr::drop_in_place((&raw mut (*gc_box).value).cast::<T>()) };
}

/// # Safety
/// `header` must be a `GcBox<T>` from `Gc::new`, whose value has been dropped, with no handles left.
unsafe fn dealloc_box<T>(header: NonNull<Header>) {
    // SAFETY: allocated by `Box::new` in `Gc::new`; the `ManuallyDrop` value isn't dropped again.
    drop(unsafe { Box::from_raw(header.cast::<GcBox<T>>().as_ptr()) });
}

impl<T: Trace + 'static> Gc<T> {
    pub(crate) fn new(value: T) -> Self {
        let gc_box = Box::new(GcBox {
            header: Header {
                strong: Cell::new(1),
                gc_refs: Cell::new(0),
                index: Cell::new(0),
                generation: Cell::new(Generation::Young),
                flags: Cell::new(0),
                vtable: &GcBox::<T>::VTABLE,
            },
            value: ManuallyDrop::new(value),
        });
        let ptr = Box::into_non_null(gc_box);
        let gc = Gc {
            ptr,
            _marker: PhantomData,
        };
        // The new box is safe even if a collection runs right away: this handle is an outside reference.
        if let Some(generation) = track(gc.header()) {
            run_automatic_collection(generation);
        }
        gc
    }

    fn header(&self) -> NonNull<Header> {
        self.ptr.cast()
    }

    fn header_ref(&self) -> &Header {
        // SAFETY: the box is alive while this handle exists.
        unsafe { self.ptr.cast::<Header>().as_ref() }
    }

    pub(crate) fn ptr_eq(a: &Gc<T>, b: &Gc<T>) -> bool {
        a.ptr == b.ptr
    }

    /// The box's address, for identity hashing and cycle detection
    pub(crate) fn addr(&self) -> usize {
        self.ptr.as_ptr() as usize
    }
}

impl<T: Trace + 'static> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        // SAFETY: the box (and its value) are alive while this handle exists.
        unsafe { &self.ptr.as_ref().value }
    }
}

impl<T: Trace + 'static> Clone for Gc<T> {
    fn clone(&self) -> Self {
        let strong = &self.header_ref().strong;
        strong.set(strong.get() + 1);
        Gc {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T: Trace + 'static> Drop for Gc<T> {
    fn drop(&mut self) {
        let strong = &self.header_ref().strong;
        let n = strong.get() - 1;
        strong.set(n);
        if n == 0 {
            // SAFETY: that was the last handle.
            unsafe { release(self.header()) };
        }
    }
}

// --- Freeing ---

thread_local! {
    /// Set while boxes are being freed: boxes released meanwhile are queued in `PENDING` instead of being freed
    /// recursively
    static FREEING: Cell<bool> = const { Cell::new(false) };
    static PENDING: RefCell<Vec<NonNull<Header>>> = const { RefCell::new(Vec::new()) };
}

/// Free a box with no handles left, or queue it if another is being freed.
///
/// # Safety
/// `header` must be a live, tracked box with a zero count.
unsafe fn release(header: NonNull<Header>) {
    let Ok(freeing) = FREEING.try_with(|f| f.replace(true)) else {
        // Thread teardown: free it directly
        // SAFETY: as for this function.
        return unsafe { free(header) };
    };
    if freeing {
        PENDING.with(|p| p.borrow_mut().push(header));
        return;
    }
    let _guard = FreeingGuard;
    // SAFETY: as for this function.
    unsafe { free(header) };
    drain_pending();
}

/// Free the boxes queued while others were being freed (including those that queues while this runs).
fn drain_pending() {
    while let Some(header) = PENDING.with(|p| p.borrow_mut().pop()) {
        // SAFETY: queued boxes are live, tracked and have no handles.
        unsafe { free(header) };
    }
}

/// Clears `FREEING` (even if a destructor panics).
struct FreeingGuard;

impl Drop for FreeingGuard {
    fn drop(&mut self) {
        let _ = FREEING.try_with(|f| f.set(false));
    }
}

/// # Safety
/// `header` must be a live, tracked box with no handles.
unsafe fn free(header: NonNull<Header>) {
    untrack(header);
    // SAFETY: the caller guarantees nothing else uses the box.
    unsafe {
        let vtable = header.as_ref().vtable;
        (vtable.drop_value)(header);
        (vtable.dealloc)(header);
    }
}

// --- The heap: tracked boxes by generation ---

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Generation {
    Young = 0,
    Old = 1,
}

struct Heap {
    generations: [Vec<NonNull<Header>>; 2],
    policy: Policy,
    /// A full collection runs (instead of a young one) when the old generation has this many boxes
    full_threshold: usize,
    stats: Stats,
}

impl Heap {
    fn live(&self) -> usize {
        self.generations[0].len() + self.generations[1].len()
    }
}

thread_local! {
    static HEAP: RefCell<Heap> = const {
        RefCell::new(Heap {
            generations: [Vec::new(), Vec::new()],
            policy: Policy::HeapGrowth,
            full_threshold: MIN_FULL_THRESHOLD,
            stats: Stats { young: 0, full: 0, explicit: 0 },
        })
    };
}

/// Start tracking a new box (in the young generation). Returns the collection to run, if one is due.
fn track(header: NonNull<Header>) -> Option<Generation> {
    HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        let young = &mut heap.generations[0];
        // SAFETY: the box was just allocated.
        unsafe { header.as_ref() }.index.set(young.len());
        young.push(header);
        (heap.policy == Policy::HeapGrowth && heap.generations[0].len() >= YOUNG_THRESHOLD).then(|| {
            if heap.generations[1].len() >= heap.full_threshold {
                Generation::Old
            } else {
                Generation::Young
            }
        })
    })
}

fn untrack(header: NonNull<Header>) {
    let _ = HEAP.try_with(|heap| {
        let mut heap = heap.borrow_mut();
        // SAFETY: tracked boxes are alive.
        let h = unsafe { header.as_ref() };
        let list = &mut heap.generations[h.generation.get() as usize];
        let index = h.index.get();
        list.swap_remove(index);
        if let Some(moved) = list.get(index) {
            // SAFETY: as above.
            unsafe { moved.as_ref() }.index.set(index);
        }
    });
}

// --- Collection ---

fn run_automatic_collection(generation: Generation) {
    if run_collection(generation) {
        HEAP.with(|heap| {
            let stats = &mut heap.borrow_mut().stats;
            match generation {
                Generation::Young => stats.young += 1,
                Generation::Old => stats.full += 1,
            }
        });
    }
}

/// Collect the young generation, or (`Old`) everything. Returns false if a collection or freeing was already
/// under way, so nothing was done.
fn run_collection(generation: Generation) -> bool {
    if FREEING.get() {
        return false;
    }
    let garbage = HEAP.with(|heap| {
        let mut heap = heap.try_borrow_mut().ok()?;
        let garbage = find_garbage(&mut heap, generation);
        if generation == Generation::Old {
            heap.full_threshold = (heap.live() * 2).max(MIN_FULL_THRESHOLD);
        }
        Some(garbage)
    });
    let Some(garbage) = garbage else {
        return false;
    };
    // SAFETY: `find_garbage` returns unreachable boxes, untracked.
    unsafe { free_garbage(garbage) };
    true
}

/// Find the unreachable boxes in the generations up to `generation`, untrack them and return them. Survivors
/// move to the old generation.
fn find_garbage(heap: &mut Heap, generation: Generation) -> Vec<NonNull<Header>> {
    let collected = match generation {
        Generation::Young => &heap.generations[..1],
        Generation::Old => &heap.generations[..],
    };
    // SAFETY (for every `as_ref` below): tracked boxes, and the boxes they reference, are alive, and nothing is
    // freed until this returns.
    let set = || collected.iter().flatten().map(|h| unsafe { h.as_ref() });
    let mut tracer = Tracer::default();

    // 1. Copy the reference counts
    for h in set() {
        h.gc_refs.set(h.strong.get());
        h.flags.set(IN_SET);
    }
    // 2. Subtract references from within the set
    for h in set() {
        match tracer.trace(NonNull::from(h)) {
            Some(edges) => {
                for edge in edges {
                    let e = unsafe { edge.as_ref() };
                    if e.flags.get() & IN_SET != 0 {
                        e.gc_refs.set(e.gc_refs.get() - 1);
                    }
                }
            }
            None => h.flags.set(h.flags.get() | UNTRACEABLE),
        }
    }
    // 3. Mark everything reachable from boxes referenced from outside
    let mut stack: Vec<&Header> = set()
        .filter(|h| h.gc_refs.get() > 0 || h.flags.get() & UNTRACEABLE != 0)
        .collect();
    for h in &stack {
        h.flags.set(h.flags.get() | REACHABLE);
    }
    let mut consistent = true;
    while let Some(h) = stack.pop() {
        if h.flags.get() & UNTRACEABLE != 0 {
            continue;
        }
        let Some(edges) = tracer.trace(NonNull::from(h)) else {
            // Traced fine a moment ago, so this can't happen; but if it did, its children might be wrongly
            // considered garbage
            consistent = false;
            break;
        };
        for edge in edges {
            let e = unsafe { edge.as_ref() };
            let flags = e.flags.get();
            if flags & IN_SET != 0 && flags & REACHABLE == 0 {
                e.flags.set(flags | REACHABLE);
                stack.push(e);
            }
        }
    }
    // 4. Separate the garbage; survivors become old
    let mut garbage = vec![];
    let mut old = match generation {
        Generation::Young => std::mem::take(&mut heap.generations[1]),
        Generation::Old => Vec::with_capacity(heap.live()),
    };
    for list in &mut heap.generations[..=generation as usize] {
        for header in list.drain(..) {
            let h = unsafe { header.as_ref() };
            let reachable = !consistent || h.flags.get() & REACHABLE != 0;
            h.flags.set(0);
            if reachable {
                h.index.set(old.len());
                h.generation.set(Generation::Old);
                old.push(header);
            } else {
                garbage.push(header);
            }
        }
    }
    heap.generations[1] = old;
    garbage
}

/// Free unreachable boxes. Every reference to them is from another of them, so: hold each (so none is freed
/// while others' values are being dropped), drop all their values (releasing those references), then free
/// the boxes.
///
/// # Safety
/// `garbage` must be live, untracked boxes referenced only from each other.
unsafe fn free_garbage(garbage: Vec<NonNull<Header>>) {
    if garbage.is_empty() {
        return;
    }
    // SAFETY (for every `as_ref`): the boxes stay allocated until the last loop.
    let header = |h: &NonNull<Header>| unsafe { h.as_ref() };
    FREEING.set(true);
    let _guard = FreeingGuard;
    for h in &garbage {
        let strong = &header(h).strong;
        strong.set(strong.get() + 1);
    }
    for h in &garbage {
        // SAFETY: unreachable, so nothing will use the value again.
        unsafe { (header(h).vtable.drop_value)(*h) };
    }
    for h in &garbage {
        let strong = &header(h).strong;
        strong.set(strong.get() - 1);
        if strong.get() == 0 {
            // SAFETY: the value is dropped and no handles remain.
            unsafe { (header(h).vtable.dealloc)(*h) };
        } else {
            // A handle from outside that wasn't counted as one: a `Trace` impl reported a `Gc` it doesn't own.
            // Leak the box rather than free it under that handle (its value is gone either way).
            debug_assert!(false, "cycle collector: unreachable box still has handles");
        }
    }
    // Boxes that only the garbage referenced
    drain_pending();
}
