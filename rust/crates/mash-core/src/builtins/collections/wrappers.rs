//! List-method wrappers and registration lists for the ordering, grouping, selection and List-specific builtins.

use super::Methodised;
use super::grouping::{CountByFunction, CountsFunction, GroupByFunction};
use super::join_by::JoinByFunction;
use super::list_methods::ListNewFunction;
use super::ordering::{MaxByFunction, MaxFunction, MinByFunction, MinFunction, SortByFunction, SortFunction};
use super::parse::{ParseFunction, ParseTableFunction, ScanFunction};
use super::rename::{DefaultFunction, RenameFunction, UpdateFunction};
use super::select::{DeselectFunction, SelectFunction};
use super::transpose::TransposeFunction;
use crate::function::BuiltinFunction;

pub(crate) const ORDERING_FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &CountByFunction,
    &CountsFunction,
    &DefaultFunction,
    &DeselectFunction,
    &GroupByFunction,
    &JoinByFunction,
    &MaxByFunction,
    &MaxFunction,
    &MinByFunction,
    &MinFunction,
    &ParseFunction,
    &ParseTableFunction,
    &RenameFunction,
    &ScanFunction,
    &SelectFunction,
    &SortByFunction,
    &SortFunction,
    &TransposeFunction,
    &UpdateFunction,
];

pub(crate) static COUNT_BY_METHOD: Methodised = Methodised::new(&CountByFunction, "sequence", &[], false);
pub(crate) static COUNTS_METHOD: Methodised = Methodised::new(&CountsFunction, "sequence", &[], false);
pub(crate) static GROUP_BY_METHOD: Methodised = Methodised::new(&GroupByFunction, "sequence", &[], false);
pub(crate) static MAX_BY_METHOD: Methodised = Methodised::new(&MaxByFunction, "sequence", &[], false);
pub(crate) static MAX_METHOD: Methodised = Methodised::new(&MaxFunction, "items", &[], false);
pub(crate) static MIN_BY_METHOD: Methodised = Methodised::new(&MinByFunction, "sequence", &[], false);
pub(crate) static MIN_METHOD: Methodised = Methodised::new(&MinFunction, "items", &[], false);
pub(crate) static PARSE_METHOD: Methodised = Methodised::new(&ParseFunction, "input", &[], false);
pub(crate) static PARSE_TABLE_METHOD: Methodised = Methodised::new(&ParseTableFunction, "input", &[], false);
pub(crate) static SCAN_METHOD: Methodised = Methodised::new(&ScanFunction, "input", &[], false);
pub(crate) static SORT_BY_METHOD: Methodised = Methodised::new(&SortByFunction, "sequence", &[], false);
pub(crate) static SORT_METHOD: Methodised = Methodised::new(&SortFunction, "sequence", &[], false);
pub(crate) static TRANSPOSE_METHOD: Methodised = Methodised::new(&TransposeFunction, "sequence", &[], false);

pub(crate) const LIST_STATIC_METHODS: &[&dyn BuiltinFunction] = &[&ListNewFunction];
