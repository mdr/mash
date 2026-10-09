//! The state of each kind of browser view (Scala `TwoDTableBrowserState`, `SingleObjectTableBrowserState`,
//! `ObjectTreeBrowserState`, `TextLinesBrowserState`, `HelpBrowserState` and `ValueBrowserState`): where the
//! cursor is, which rows are shown, marked rows and any search. Methods that move the cursor take the
//! number of rows on the terminal, to keep the cursor in view.

use std::collections::{BTreeSet, HashMap};

use mash_core::builtins::meta::value_to_expression;
use mash_core::eval::Ctx;
use mash_core::value::{List, Object, Value};
use mash_syntax::expression_combiner::combine_safely;

use super::model::{HelpModel, TextLinesModel, TreeModel, ValueModel};
use super::search::SearchState;
use super::{ExpressionState, SelectionInfo, allows_nullary, fit_window, safe_property};
use crate::view::help::LinkPath;
use crate::view::object::SingleObjectTableModel;
use crate::view::table::TwoDTableModel;
use crate::view::tree::{TreeChoice, TreeNode, TreePath};

/// `current` moved by `delta`, wrapping round `count`
fn wrap(current: usize, delta: isize, count: usize) -> usize {
    (current as isize + delta).rem_euclid(count as isize) as usize
}

/// The row a page down from `current` (Scala `nextPage`)
fn page_down(current: usize, window: usize, count: usize) -> usize {
    (current + window).saturating_sub(1).min(count.saturating_sub(1))
}

/// The row a page up from `current` (Scala `previousPage`, which goes up one more than a page)
fn page_up(current: usize, window: usize) -> usize {
    current.saturating_sub(window + 1)
}

/// The expression for the rows of a list or object at `indices` (when several are marked): `path | [_[0],
/// _[2]]` or `path | select --a --c`, and their values
fn multiple_selection(path: &str, value: &Value, indices: &[usize]) -> Option<SelectionInfo> {
    match value {
        Value::List(xs) => {
            let items = xs.snapshot();
            let lookups: Vec<String> = indices.iter().map(|i| format!("_[{i}]")).collect();
            let selected = indices.iter().filter_map(|&i| items.get(i).cloned()).collect();
            Some(SelectionInfo {
                path: combine_safely(path, &format!(" | [{}]", lookups.join(", "))),
                value: Value::List(List::new(selected)),
            })
        }
        Value::Object(o) => Some(select_fields(path, o, indices)),
        _ => None,
    }
}

/// The fields of `object` at `indices`: `path | select --a --c`, and an object of them
fn select_fields(path: &str, object: &Object, indices: &[usize]) -> SelectionInfo {
    let entries = object.entries();
    let selected: Vec<(Value, Value)> = indices.iter().filter_map(|&i| entries.get(i).cloned()).collect();
    let flags: Vec<String> = selected
        .iter()
        .map(|(field, _)| format!("--{}", mash_core::eval::render(field)))
        .collect();
    SelectionInfo {
        path: combine_safely(path, &format!(" | select {}", flags.join(" "))),
        value: Value::Object(Object::from_pairs(selected)),
    }
}

// ------------------------------------------------------------------------------------------------ 2D table

/// A 2D table (Scala `TwoDTableBrowserState`).
#[derive(Clone, PartialEq, Debug)]
pub struct TwoDTableState {
    pub model: TwoDTableModel,
    pub current_row: usize,
    /// The first row shown
    pub first_row: usize,
    /// The selected column, if any: 0 is the row labels, and the data columns follow
    pub current_column: Option<usize>,
    pub marked: BTreeSet<usize>,
    pub path: String,
    /// The ids of columns the user has hidden
    pub hidden_columns: Vec<usize>,
    pub search: Option<SearchState>,
    pub expression: Option<ExpressionState>,
}

impl TwoDTableState {
    pub fn new(model: TwoDTableModel, path: String) -> TwoDTableState {
        TwoDTableState {
            model,
            current_row: 0,
            first_row: 0,
            current_column: None,
            marked: BTreeSet::new(),
            path,
            hidden_columns: vec![],
            search: None,
            expression: None,
        }
    }

    /// How many rows fit: the terminal less an upper status line, three header rows, a footer row and a
    /// lower status line
    pub fn window_size(rows: usize) -> usize {
        rows.saturating_sub(6)
    }

    fn number_of_rows(&self) -> usize {
        self.model.number_of_rows()
    }

    fn number_of_columns(&self) -> usize {
        self.model.number_of_columns()
    }

    fn fit(&mut self, rows: usize) {
        self.first_row = fit_window(self.current_row, self.first_row, Self::window_size(rows));
    }

    fn go_to_row(&mut self, row: usize, rows: usize) {
        self.current_row = row;
        self.fit(rows);
    }

    pub fn next_item(&mut self, rows: usize) {
        self.adjust_row(1, rows);
    }

    pub fn previous_item(&mut self, rows: usize) {
        self.adjust_row(-1, rows);
    }

    fn adjust_row(&mut self, delta: isize, rows: usize) {
        if self.number_of_rows() > 0 {
            self.current_row = wrap(self.current_row, delta, self.number_of_rows());
        }
        self.fit(rows);
    }

    pub fn next_page(&mut self, rows: usize) {
        let row = page_down(self.current_row, Self::window_size(rows), self.number_of_rows());
        self.go_to_row(row, rows);
    }

    pub fn previous_page(&mut self, rows: usize) {
        let row = page_up(self.current_row, Self::window_size(rows));
        self.go_to_row(row, rows);
    }

    pub fn first_item(&mut self, rows: usize) {
        self.go_to_row(0, rows);
    }

    pub fn last_item(&mut self, rows: usize) {
        self.go_to_row(self.number_of_rows().saturating_sub(1), rows);
    }

    pub fn next_column(&mut self) {
        self.adjust_column(1);
    }

    pub fn previous_column(&mut self) {
        self.adjust_column(-1);
    }

    /// Select the next or previous column, wrapping round; with no column selected, the first or last
    fn adjust_column(&mut self, delta: isize) {
        let n = self.number_of_columns();
        if n == 0 {
            return;
        }
        self.current_column = Some(match self.current_column {
            Some(column) => wrap(column, delta, n),
            None if delta > 0 => delta as usize - 1,
            None => wrap(0, delta, n),
        });
    }

    pub fn unfocus_column(&mut self) {
        self.current_column = None;
    }

    pub fn first_column(&mut self) {
        self.current_column = Some(0);
    }

    pub fn last_column(&mut self) {
        self.current_column = Some(self.number_of_columns().saturating_sub(1));
    }

    pub fn toggle_mark(&mut self) {
        if !self.marked.remove(&self.current_row) {
            self.marked.insert(self.current_row);
        }
    }

    /// Start an incremental search (deselecting any column)
    pub fn begin_search(&mut self) {
        self.search = Some(SearchState::empty());
        self.current_column = None;
    }

    /// Search for `query`, keeping the case sensitivity, and move to the first hit from the current row
    pub fn set_search(&mut self, query: &str, rows: usize) {
        let ignore_case = self.search.as_ref().is_none_or(|s| s.ignore_case);
        self.run_search(query, ignore_case, rows);
    }

    pub fn toggle_case(&mut self, rows: usize) {
        if let Some(search) = &self.search {
            let (query, ignore_case) = (search.query.clone(), !search.ignore_case);
            self.run_search(&query, ignore_case, rows);
        }
    }

    fn run_search(&mut self, query: &str, ignore_case: bool, rows: usize) {
        let cells = self.model.rows.iter().enumerate().flat_map(|(row, r)| {
            r.cells
                .iter()
                .enumerate()
                .map(move |(column, cell)| (row, column, cell.rendered.as_str()))
        });
        let search = SearchState::new(query, ignore_case, cells);
        let row = search.first_hit_from(self.current_row).unwrap_or(self.current_row);
        self.search = Some(search);
        self.go_to_row(row, rows);
    }

    pub fn next_hit(&mut self, rows: usize) {
        if let Some(row) = self.search.as_ref().and_then(|s| s.next_hit(self.current_row)) {
            self.go_to_row(row, rows);
        }
    }

    pub fn previous_hit(&mut self, rows: usize) {
        if let Some(row) = self.search.as_ref().and_then(|s| s.previous_hit(self.current_row)) {
            self.go_to_row(row, rows);
        }
    }

    pub fn stop_searching(&mut self) {
        self.search = None;
    }

    /// The selection: several marked rows; one marked row; or the current row, or the selected cell in it
    pub fn selection_info(&self) -> Option<SelectionInfo> {
        let marked: Vec<usize> = self.marked.iter().copied().collect();
        match marked.as_slice() {
            [] => self.row_selection(self.current_row, true),
            [row] => self.row_selection(*row, false),
            rows => multiple_selection(&self.path, &self.model.raw_value, rows),
        }
    }

    fn row_selection(&self, index: usize, include_cell: bool) -> Option<SelectionInfo> {
        let row = self.model.rows.get(index)?;
        let row_path = row.fetch.path(&self.path);
        let cell_selection = || {
            let column_index = self.current_column?;
            let value = row.cells.get(column_index)?.raw_value.clone()?;
            let path = match &self.model.columns[column_index].fetch {
                // The row label: the field name or index itself
                None => combine_safely(&self.path, &format!(" | _ => {}", value_to_expression(&value)?)),
                Some(fetch) => fetch.path(&row_path),
            };
            Some(SelectionInfo { path, value })
        };
        include_cell.then(cell_selection).flatten().or_else(|| {
            Some(SelectionInfo {
                path: row_path.clone(),
                value: row.raw_value.clone(),
            })
        })
    }
}

// ------------------------------------------------------------------------------------------ single object

/// A table of an object's fields (Scala `SingleObjectTableBrowserState`).
#[derive(Clone, PartialEq, Debug)]
pub struct SingleObjectState {
    pub model: SingleObjectTableModel,
    pub current_row: usize,
    pub first_row: usize,
    pub marked: BTreeSet<usize>,
    pub path: String,
    pub search: Option<SearchState>,
    pub expression: Option<ExpressionState>,
}

impl SingleObjectState {
    pub fn new(model: SingleObjectTableModel, path: String) -> SingleObjectState {
        SingleObjectState {
            model,
            current_row: 0,
            first_row: 0,
            marked: BTreeSet::new(),
            path,
            search: None,
            expression: None,
        }
    }

    /// How many rows fit: the terminal less an upper status line, the header rows (one, or three with a
    /// class name), a footer row and a lower status line
    pub fn window_size(&self, rows: usize) -> usize {
        let header = if self.model.class_name.is_some() { 3 } else { 1 };
        rows.saturating_sub(3 + header)
    }

    fn number_of_rows(&self) -> usize {
        self.model.number_of_rows()
    }

    fn go_to_row(&mut self, row: usize, rows: usize) {
        self.current_row = row;
        self.first_row = fit_window(self.current_row, self.first_row, self.window_size(rows));
    }

    pub fn next_item(&mut self, rows: usize) {
        self.adjust_row(1, rows);
    }

    pub fn previous_item(&mut self, rows: usize) {
        self.adjust_row(-1, rows);
    }

    fn adjust_row(&mut self, delta: isize, rows: usize) {
        if self.number_of_rows() > 0 {
            let row = wrap(self.current_row, delta, self.number_of_rows());
            self.go_to_row(row, rows);
        }
    }

    pub fn next_page(&mut self, rows: usize) {
        let row = page_down(self.current_row, self.window_size(rows), self.number_of_rows());
        self.go_to_row(row, rows);
    }

    pub fn previous_page(&mut self, rows: usize) {
        let row = page_up(self.current_row, self.window_size(rows));
        self.go_to_row(row, rows);
    }

    pub fn first_item(&mut self, rows: usize) {
        self.go_to_row(0, rows);
    }

    pub fn last_item(&mut self, rows: usize) {
        self.go_to_row(self.number_of_rows().saturating_sub(1), rows);
    }

    pub fn toggle_mark(&mut self) {
        if !self.marked.remove(&self.current_row) {
            self.marked.insert(self.current_row);
        }
    }

    pub fn begin_search(&mut self) {
        self.search = Some(SearchState::empty());
    }

    pub fn set_search(&mut self, query: &str, rows: usize) {
        let ignore_case = self.search.as_ref().is_none_or(|s| s.ignore_case);
        self.run_search(query, ignore_case, rows);
    }

    pub fn toggle_case(&mut self, rows: usize) {
        if let Some(search) = &self.search {
            let (query, ignore_case) = (search.query.clone(), !search.ignore_case);
            self.run_search(&query, ignore_case, rows);
        }
    }

    fn run_search(&mut self, query: &str, ignore_case: bool, rows: usize) {
        let cells = self
            .model
            .fields
            .iter()
            .enumerate()
            .flat_map(|(row, (field, value))| [(row, 0, field.as_str()), (row, 1, value.as_str())]);
        let search = SearchState::new(query, ignore_case, cells);
        let row = search.first_hit_from(self.current_row).unwrap_or(self.current_row);
        self.search = Some(search);
        self.go_to_row(row, rows);
    }

    pub fn next_hit(&mut self, rows: usize) {
        if let Some(row) = self.search.as_ref().and_then(|s| s.next_hit(self.current_row)) {
            self.go_to_row(row, rows);
        }
    }

    pub fn previous_hit(&mut self, rows: usize) {
        if let Some(row) = self.search.as_ref().and_then(|s| s.previous_hit(self.current_row)) {
            self.go_to_row(row, rows);
        }
    }

    pub fn stop_searching(&mut self) {
        self.search = None;
    }

    /// The selection: several marked fields, one marked field, or the current field
    pub fn selection_info(&self, ctx: &Ctx) -> Option<SelectionInfo> {
        let marked: Vec<usize> = self.marked.iter().copied().collect();
        match marked.as_slice() {
            [] => self.field_selection(ctx, self.current_row),
            [row] => self.field_selection(ctx, *row),
            rows => multiple_selection(&self.path, &self.model.raw_value, rows),
        }
    }

    fn field_selection(&self, ctx: &Ctx, index: usize) -> Option<SelectionInfo> {
        let (field, value) = self.model.raw_fields.get(index)?;
        Some(SelectionInfo {
            path: safe_property(&self.path, field, allows_nullary(ctx, value)),
            value: value.clone(),
        })
    }
}

// ------------------------------------------------------------------------------------------------ tree

/// A tree of nested lists and objects (Scala `ObjectTreeBrowserState`). The selection is a path through the
/// tree: to an item's connector, a field's label, or a leaf value. Up and down move between the parts at
/// the same depth.
#[derive(Clone, PartialEq, Debug)]
pub struct TreeState {
    pub model: TreeModel,
    pub selection: TreePath,
    pub path: String,
    pub first_row: usize,
    pub expression: Option<ExpressionState>,
}

/// Where each selectable part of a tree is: its depth (for moving up and down) and row
#[derive(Default)]
struct TreeScan {
    depths: HashMap<TreePath, usize>,
    by_depth: HashMap<usize, Vec<TreePath>>,
    rows: HashMap<TreePath, usize>,
    row: usize,
}

impl TreeScan {
    fn record(&mut self, path: TreePath, depth: usize) {
        self.depths.insert(path.clone(), depth);
        self.by_depth.entry(depth).or_default().push(path);
    }

    /// Scala `DepthScan`
    fn scan(&mut self, node: &TreeNode, path: &TreePath, depth: usize) {
        match node {
            TreeNode::List(items, _) if !items.is_empty() => {
                for (index, item) in items.iter().enumerate() {
                    let item_path = path.descend_index(index);
                    self.rows.insert(item_path.clone(), self.row);
                    self.record(item_path.clone(), depth);
                    let nested = !matches!(item, TreeNode::Leaf(..)) && !item.is_empty_container();
                    self.scan(item, &item_path, if nested { depth + 1 } else { depth });
                }
            }
            TreeNode::Object(fields, _) if !fields.is_empty() => {
                for (field, value) in fields {
                    let item_path = path.descend_field(field);
                    self.rows.insert(item_path.clone(), self.row);
                    self.record(item_path.clone(), depth);
                    let label_path = item_path.onto_field_label();
                    self.rows.insert(label_path.clone(), self.row);
                    if !matches!(value, TreeNode::Leaf(..)) && !value.is_empty_container() {
                        self.row += 1;
                    }
                    self.record(label_path.clone(), depth + 1);
                    self.scan(value, &label_path, depth + 1);
                }
            }
            _ => {
                let value_path = path.onto_value();
                self.rows.insert(value_path.clone(), self.row);
                self.row += 1;
                self.record(value_path, depth + 1);
            }
        }
    }
}

impl TreeState {
    /// Browse a tree, selecting the first item of its root (which must be a non-empty list or object)
    pub fn new(model: TreeModel, path: String) -> TreeState {
        let first = match &model.root {
            TreeNode::List(..) => TreeChoice::Index(0),
            TreeNode::Object(fields, _) => {
                TreeChoice::Field(fields.first().map(|(f, _)| f.clone()).unwrap_or_default())
            }
            TreeNode::Leaf(..) => TreeChoice::OntoValue,
        };
        TreeState {
            model,
            selection: TreePath(vec![first]),
            path,
            first_row: 0,
            expression: None,
        }
    }

    fn scan(&self) -> TreeScan {
        let mut scan = TreeScan::default();
        scan.scan(&self.model.root, &TreePath::default(), 0);
        scan
    }

    fn node<'a>(node: &'a TreeNode, choices: &[TreeChoice]) -> Option<&'a TreeNode> {
        let Some((choice, rest)) = choices.split_first() else {
            return Some(node);
        };
        match (choice, node) {
            (TreeChoice::OntoFieldLabel, _) => Self::node(node, rest),
            (TreeChoice::OntoValue, _) => Some(node),
            (TreeChoice::Index(i), TreeNode::List(items, _)) => Self::node(items.get(*i)?, rest),
            (TreeChoice::Field(field), TreeNode::Object(fields, _)) => {
                let (_, child) = fields.iter().find(|(f, _)| f == field)?;
                Self::node(child, rest)
            }
            _ => None,
        }
    }

    fn selected_node(&self) -> Option<&TreeNode> {
        Self::node(&self.model.root, &self.selection.0)
    }

    /// Into the selected part: from an item to its first child (or value), from a field to its label
    pub fn right(&mut self) {
        let next = match self.selection.0.last() {
            Some(TreeChoice::Field(_)) => self.selection.onto_field_label(),
            Some(TreeChoice::Index(_) | TreeChoice::OntoFieldLabel) => match self.selected_node() {
                Some(TreeNode::List(items, _)) if !items.is_empty() => self.selection.descend_index(0),
                Some(TreeNode::Object(fields, _)) if !fields.is_empty() => self.selection.descend_field(&fields[0].0),
                _ => self.selection.onto_value(),
            },
            _ => return,
        };
        self.selection = next;
    }

    /// Out of the selected part
    pub fn left(&mut self) {
        if self.selection.0.len() > 1 {
            self.selection = self.selection.ascend();
        }
    }

    pub fn next_item(&mut self, rows: usize) {
        self.up_down(1, rows);
    }

    pub fn previous_item(&mut self, rows: usize) {
        self.up_down(-1, rows);
    }

    /// The next or previous part at the same depth, wrapping round
    fn up_down(&mut self, delta: isize, rows: usize) {
        let scan = self.scan();
        let Some(depth) = scan.depths.get(&self.selection) else {
            return;
        };
        let paths = &scan.by_depth[depth];
        if let Some(i) = paths.iter().position(|p| *p == self.selection) {
            self.selection = paths[wrap(i, delta, paths.len())].clone();
        }
        let row = scan.rows.get(&self.selection).copied().unwrap_or(0);
        self.first_row = fit_window(row, self.first_row, Self::window_size(rows));
    }

    /// How many rows fit: the terminal less two status lines
    pub fn window_size(rows: usize) -> usize {
        rows.saturating_sub(2)
    }

    /// The selected part's value, and an expression for it: the path with each field and index along the
    /// way
    pub fn selection_info(&self) -> Option<SelectionInfo> {
        let mut path = self.path.clone();
        for choice in &self.selection.0 {
            match choice {
                TreeChoice::Index(i) => path = combine_safely(&path, &format!("[{i}]")),
                TreeChoice::Field(field) => path = combine_safely(&path, &format!(".{field}")),
                _ => {}
            }
        }
        Some(SelectionInfo {
            path,
            value: self.selected_node()?.raw_value().clone(),
        })
    }
}

// ------------------------------------------------------------------------------------------------ text lines

/// A list, a line per item (Scala `TextLinesBrowserState`).
#[derive(Clone, PartialEq, Debug)]
pub struct TextLinesState {
    pub model: TextLinesModel,
    pub path: String,
    pub selected_row: usize,
    pub first_row: usize,
    pub expression: Option<ExpressionState>,
}

impl TextLinesState {
    pub fn new(model: TextLinesModel, path: String) -> TextLinesState {
        TextLinesState {
            model,
            path,
            selected_row: 0,
            first_row: 0,
            expression: None,
        }
    }

    /// How many rows fit: the terminal less two status lines
    pub fn window_size(rows: usize) -> usize {
        rows.saturating_sub(2)
    }

    fn size(&self) -> usize {
        self.model.lines.len()
    }

    fn go_to_row(&mut self, row: usize, rows: usize) {
        self.selected_row = row;
        self.first_row = fit_window(row, self.first_row, Self::window_size(rows));
    }

    pub fn next_item(&mut self, rows: usize) {
        if self.size() > 0 {
            self.go_to_row(wrap(self.selected_row, 1, self.size()), rows);
        }
    }

    pub fn previous_item(&mut self, rows: usize) {
        if self.size() > 0 {
            self.go_to_row(wrap(self.selected_row, -1, self.size()), rows);
        }
    }

    pub fn first_item(&mut self, rows: usize) {
        self.go_to_row(0, rows);
    }

    pub fn last_item(&mut self, rows: usize) {
        self.go_to_row(self.size().saturating_sub(1), rows);
    }

    pub fn next_page(&mut self, rows: usize) {
        let row = page_down(self.selected_row, Self::window_size(rows), self.size());
        self.go_to_row(row, rows);
    }

    pub fn previous_page(&mut self, rows: usize) {
        let row = page_up(self.selected_row, Self::window_size(rows));
        self.go_to_row(row, rows);
    }

    pub fn selection_info(&self) -> Option<SelectionInfo> {
        let Value::List(xs) = &self.model.raw_value else {
            return None;
        };
        let value = xs.snapshot().get(self.selected_row)?.clone();
        Some(SelectionInfo {
            path: combine_safely(&self.path, &format!("[{}]", self.selected_row)),
            value,
        })
    }
}

// ------------------------------------------------------------------------------------------------ help

/// Help, with links to other help (Scala `HelpBrowserState`).
#[derive(Clone, PartialEq, Debug)]
pub struct HelpState {
    pub model: HelpModel,
    pub current_row: usize,
    pub first_row: usize,
    pub path: String,
    pub expression: Option<ExpressionState>,
}

impl HelpState {
    pub fn new(model: HelpModel, path: String) -> HelpState {
        HelpState {
            model,
            current_row: 0,
            first_row: 0,
            path,
            expression: None,
        }
    }

    /// How many rows fit: the terminal less two status lines and two border lines
    pub fn window_size(rows: usize) -> usize {
        rows.saturating_sub(4)
    }

    fn number_of_rows(&self) -> usize {
        self.model.lines.len()
    }

    fn go_to_row(&mut self, row: usize, rows: usize) {
        self.current_row = row;
        self.first_row = fit_window(row, self.first_row, Self::window_size(rows));
    }

    /// Up or down a line, stopping at the ends
    fn adjust_row(&mut self, delta: isize, rows: usize) {
        if self.number_of_rows() > 0 {
            let row = (self.current_row as isize + delta).clamp(0, self.number_of_rows() as isize - 1);
            self.go_to_row(row as usize, rows);
        }
    }

    pub fn next_item(&mut self, rows: usize) {
        self.adjust_row(1, rows);
    }

    pub fn previous_item(&mut self, rows: usize) {
        self.adjust_row(-1, rows);
    }

    pub fn first_item(&mut self, rows: usize) {
        self.go_to_row(0, rows);
    }

    pub fn last_item(&mut self, rows: usize) {
        self.go_to_row(self.number_of_rows().saturating_sub(1), rows);
    }

    pub fn next_page(&mut self, rows: usize) {
        let row = page_down(self.current_row, Self::window_size(rows), self.number_of_rows());
        self.go_to_row(row, rows);
    }

    pub fn previous_page(&mut self, rows: usize) {
        let row = page_up(self.current_row, Self::window_size(rows));
        self.go_to_row(row, rows);
    }

    /// The link on the current line, if any
    pub fn current_link(&self) -> Option<&crate::view::help::HelpLink> {
        self.model.links.iter().find(|link| link.line == self.current_row)
    }

    /// The value linked to from the current line, if there's a link on it
    pub fn selection_info(&self) -> Option<SelectionInfo> {
        let link = self.current_link()?;
        let path = match &link.path {
            LinkPath::Absolute(path) => path.clone(),
            LinkPath::Relative(fragment) => combine_safely(&self.path, fragment),
        };
        Some(SelectionInfo {
            path,
            value: link.target.clone(),
        })
    }
}

// ------------------------------------------------------------------------------------------------ value

/// Any other value, as text (Scala `ValueBrowserState`). Nothing in it can be selected.
#[derive(Clone, PartialEq, Debug)]
pub struct ValueState {
    pub model: ValueModel,
    pub path: String,
    pub expression: Option<ExpressionState>,
}

impl ValueState {
    pub fn new(model: ValueModel, path: String) -> ValueState {
        ValueState {
            model,
            path,
            expression: None,
        }
    }
}
