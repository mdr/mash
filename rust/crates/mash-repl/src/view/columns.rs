//! Sharing out the width of a table between its columns (Scala `ColumnAllocator`).

/// What a column wants: its share of any space it can't have in full, and the width that would fit all of
/// its contents.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ColumnRequest {
    pub weight: f64,
    pub requested: usize,
}

/// The width of each column (in the same order), together at most `available`.
///
/// Space is shared out by weight. Every column whose share covers what it asked for gets exactly that, and
/// the space left over is shared among the rest in the same way, until no more columns can be satisfied;
/// then the rest get their shares (the first of them also getting any rounding remainder). Weights must be
/// positive.
pub fn allocate_columns(columns: &[ColumnRequest], available: usize) -> Vec<usize> {
    let mut widths = vec![0; columns.len()];
    let mut remaining: Vec<usize> = (0..columns.len()).collect();
    let mut available = available;
    loop {
        let shares = shares_by_weight(columns, &remaining, available);
        let (satisfied, unsatisfied): (Vec<_>, Vec<_>) = remaining
            .iter()
            .zip(&shares)
            .partition(|&(&i, &share)| share >= columns[i].requested);
        for &(&i, _) in &satisfied {
            widths[i] = columns[i].requested;
        }
        let used: usize = satisfied.iter().map(|&(&i, _)| columns[i].requested).sum();
        remaining = unsatisfied.into_iter().map(|(&i, _)| i).collect();
        if used == 0 {
            // No progress (as in Scala, this includes satisfying only columns that asked for nothing)
            for (&i, share) in remaining.iter().zip(shares_by_weight(columns, &remaining, available)) {
                widths[i] = share;
            }
            return widths;
        }
        available -= used;
    }
}

/// `available` shared out among the given columns in proportion to their weights, rounding down; the first
/// column gets the remainder
fn shares_by_weight(columns: &[ColumnRequest], ids: &[usize], available: usize) -> Vec<usize> {
    if ids.is_empty() {
        return vec![];
    }
    let total_weight: f64 = ids.iter().map(|&i| columns[i].weight).sum();
    let mut shares: Vec<usize> = ids
        .iter()
        .map(|&i| (available as f64 * columns[i].weight / total_weight) as usize)
        .collect();
    let excess = available.saturating_sub(shares.iter().sum());
    shares[0] += excess;
    shares
}
