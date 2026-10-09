//! Ported from `view/printer/ColumnAllocatorTest.scala`, plus cases for the other branches.

use mash_repl::view::columns::{ColumnRequest, allocate_columns};

fn request(weight: f64, requested: usize) -> ColumnRequest {
    ColumnRequest { weight, requested }
}

#[test]
fn a_column_that_fits_gets_what_it_asks_for() {
    assert_eq!(allocate_columns(&[request(1.0, 5)], 5), vec![5]);
}

#[test]
fn space_left_by_satisfied_columns_goes_to_the_rest() {
    assert_eq!(
        allocate_columns(&[request(1.0, 10), request(100.0, 10)], 15),
        vec![5, 10]
    );
}

#[test]
fn unsatisfiable_columns_share_by_weight_with_the_remainder_going_first() {
    assert_eq!(allocate_columns(&[request(1.0, 50), request(2.0, 50)], 10), vec![4, 6]);
    assert_eq!(
        allocate_columns(&[request(1.0, 50), request(1.0, 50), request(1.0, 50)], 10),
        vec![4, 3, 3]
    );
}

#[test]
fn columns_never_get_more_than_they_ask_for_once_satisfied() {
    assert_eq!(allocate_columns(&[request(1.0, 2), request(1.0, 3)], 80), vec![2, 3]);
}

#[test]
fn no_space() {
    assert_eq!(allocate_columns(&[request(1.0, 2), request(1.0, 3)], 0), vec![0, 0]);
    assert_eq!(allocate_columns(&[], 10), Vec::<usize>::new());
}
