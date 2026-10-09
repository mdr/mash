//! Golden tests for printing command results (`mash_repl::view`). Expected output is what Scala Mash's
//! `Viewer` prints for the same input (with styles stripped), except where a comment says otherwise.

use std::rc::Rc;

use mash_core::eval::Ctx;
use mash_core::platform::TestPlatform;
use mash_repl::screen::{Dimensions, Style, StyledString, default_colours};
use mash_repl::view::{ViewConfig, view_value};

/// Evaluate `source` and view the result on a terminal `columns` wide
fn view(columns: usize, source: &str) -> String {
    let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
    let value = ctx.run("test", source).unwrap_or_else(|e| panic!("{source}: {e}"));
    let lines = view_value(&mut ctx, &value, Dimensions::new(1000, columns), &ViewConfig::default())
        .unwrap_or_else(|e| panic!("{source}: {e}"));
    let text: Vec<String> = lines.iter().map(|l| l.forget_styling()).collect();
    text.join("\n")
}

fn check(columns: usize, source: &str, expected: &str) {
    assert_eq!(view(columns, source), expected, "viewing {source}");
}

#[test]
fn list_of_objects() {
    check(
        80,
        r#"[{a: 1, b: "hello"}, {a: 2, b: null}]"#,
        r#"╔═╤═╤═════╗
║#│a│b    ║
╟─┼─┼─────╢
║0│1│hello║
║1│2│null ║
╚═╧═╧═════╝"#,
    );
}

#[test]
fn object_with_list() {
    check(
        80,
        r#"{a: 1, b: [1,2]}"#,
        r#"╔═╤════╗
║a│1   ║
║b│1, 2║
╚═╧════╝"#,
    );
}

#[test]
fn text_lines() {
    check(
        80,
        r#"["x", null, "y"]"#,
        r#"x
null
y"#,
    );
}

#[test]
fn number() {
    check(80, r#"42"#, r#"42"#);
}

#[test]
fn tagged_values_in_a_list() {
    check(
        80,
        r#"[1023.bytes, 1536.bytes, 3.5.seconds, 2.milliseconds, true, null]"#,
        r#"[1,020B, 1.5KB, 3.5s, 2ms, true, null]"#,
    );
}

#[test]
fn list_of_lists() {
    check(
        80,
        r#"[[1,2],[3]]"#,
        r#"╔═╤═╤═╗
║#│0│1║
╟─┼─┼─╢
║0│1│2║
║1│3│ ║
╚═╧═╧═╝"#,
    );
}

#[test]
fn dissimilar_objects_are_a_single_object() {
    check(
        80,
        r#"{a: {x: 1}, b: {x: 2, y: 3}}"#,
        r#"╔═╤══════════════╗
║a│{ x: 1 }      ║
║b│{ x: 2, y: 3 }║
╚═╧══════════════╝"#,
    );
}

#[test]
fn narrow_table_truncates_cells() {
    check(
        30,
        r#"[{name: "a very long string value indeed", n: 12345, flag: true}, {name: "short", n: 1, flag: false}]"#,
        r#"╔═╤═══════════════╤═════╤════╗
║#│name           │n    │flag║
╟─┼───────────────┼─────┼────╢
║0│a very long st…│12345│✓   ║
║1│short          │1    │✗   ║
╚═╧═══════════════╧═════╧════╝"#,
    );
}

#[test]
fn narrow_object_truncates_values() {
    check(
        30,
        r#"{name: "a very long string value that goes on", other: 1}"#,
        r#"╔═════╤══════════════════════╗
║name │a very long string va…║
║other│1                     ║
╚═════╧══════════════════════╝"#,
    );
}

#[test]
fn object_with_class_name() {
    check(
        80,
        r#"maths.stats [1,2,3]"#,
        r#"╔═══════════════════╗
║       Stats       ║
╟─────────────────┬─╢
║min              │1║
║max              │3║
║mean             │2║
║standardDeviation│1║
║median           │2║
║count            │3║
╚═════════════════╧═╝"#,
    );
}

#[test]
fn object_with_class_name_narrow() {
    check(
        8,
        r#"maths.stats [1,2,3]"#,
        r#"╔══════╗
║Stats ║
╟─────┬╢
║min  │║
║max  │║
║mean │║
║stan…│║
║medi…│║
║count│║
╚═════╧╝"#,
    );
}

#[test]
fn tree() {
    check(
        30,
        r#"view.tree {a: 1, b: [1, {c: 2, d: []}], e: {}, f: {g: "hello world this is long text"}}"#,
        r#"┌─ a: 1
├─ b:
│  ├─ 1
│  └──┬─ c: 2
│     └─ d: []
├─ e: {}
└─ f:
   └─ g: hello world this is …"#,
    );
}

#[test]
fn tree_of_list() {
    check(
        80,
        r#"view.tree [1, [2, 3], {}]"#,
        r#"┌─ 1
├──┬─ 2
│  └─ 3
└─ {}"#,
    );
}

#[test]
fn tree_of_leaf() {
    check(80, r#"view.tree 1"#, r#"1"#);
}

#[test]
fn tree_of_empty_list() {
    check(80, r#"view.tree []"#, r#"[]"#);
}

#[test]
fn tree_of_nested_lists() {
    check(
        80,
        r#"view.tree [[1, [2]], {a: [3, {b: 4}]}]"#,
        r#"┌──┬─ 1
│  └──── 2
└──── a:
      ├─ 3
      └──── b: 4"#,
    );
}

#[test]
fn unit() {
    check(80, r#"()"#, r#""#);
}

#[test]
fn empty_object() {
    check(80, r#"{}"#, r#"{}"#);
}

#[test]
fn empty_list() {
    check(80, r#"[]"#, r#"[]"#);
}

#[test]
fn methods_as_cells() {
    check(
        80,
        r#"[{a: 1}, {toString: 2}]"#,
        r#"╔═╤═╤═════════════════╗
║#│a│toString         ║
╟─┼─┼─────────────────╢
║0│1│core.Any.toString║
║1│ │2                ║
╚═╧═╧═════════════════╝"#,
    );
}

#[test]
fn number_keys() {
    check(
        80,
        r#"{(1): {a: 1}, (2): {a: 2}}"#,
        r#"╔═╤═╗
║#│a║
╟─┼─╢
║1│1║
║2│2║
╚═╧═╝"#,
    );
}

#[test]
fn list_of_empty_lists() {
    check(
        80,
        r#"[[]]"#,
        r#"╔═╗
║#║
╟─╢
║0║
╚═╝"#,
    );
}

#[test]
fn list_of_empty_objects() {
    check(
        80,
        r#"[{}]"#,
        r#"╔═╗
║#║
╟─╢
║0║
╚═╝"#,
    );
}

#[test]
fn object_with_empty_object() {
    check(
        80,
        r#"{a: {}}"#,
        r#"╔═╤══╗
║a│{}║
╚═╧══╝"#,
    );
}

#[test]
fn groups() {
    check(
        80,
        r#"[1,2,3].groupBy (x => x > 1) --groups"#,
        r#"╔═╤═══╤═════╤══════╗
║#│key│count│values║
╟─┼───┼─────┼──────╢
║0│✗  │1    │1     ║
║1│✓  │2    │2, 3  ║
╚═╧═══╧═════╧══════╝"#,
    );
}

#[test]
fn object_of_lists_with_boolean_keys() {
    check(
        80,
        r#"[1,2,3].groupBy (x => x > 1)"#,
        r#"╔═╤═╤═╗
║#│0│1║
╟─┼─┼─╢
║✗│1│ ║
║✓│2│3║
╚═╧═╧═╝"#,
    );
}

#[test]
fn object_of_lists() {
    check(
        80,
        r#"{a: [1, 2], b: [3]}"#,
        r#"╔═╤═╤═╗
║#│0│1║
╟─┼─┼─╢
║a│1│2║
║b│3│ ║
╚═╧═╧═╝"#,
    );
}

#[test]
fn booleans_in_object() {
    check(
        80,
        r#"{a: true, b: false}"#,
        r#"╔═╤═╗
║a│✓║
║b│✗║
╚═╧═╝"#,
    );
}

#[test]
fn string() {
    check(80, r#""abc""#, r#"abc"#);
}

#[test]
fn null() {
    check(80, r#"null"#, r#"null"#);
}

#[test]
fn boolean() {
    check(80, r#"true"#, r#"true"#);
}

#[test]
fn seconds() {
    check(80, r#"1.5.seconds"#, r#"1.5s"#);
}

#[test]
fn list_of_numbers() {
    check(80, r#"[1, 2.5, -3]"#, r#"[1, 2.5, -3]"#);
}

#[test]
fn raw_function() {
    check(80, r#"view.raw maths.log"#, r#"maths.log"#);
}

#[test]
fn print() {
    check(80, r#"view.print [1]"#, r#"[1]"#);
}

#[test]
fn nested_views() {
    check(80, r#"view.raw (view.tree {a: 1})"#, r#"── a: 1"#);
}

#[test]
fn columns_squeezed() {
    check(
        10,
        r#"[{a: 1, b: 2, c: 3, d: 4, e: 5}]"#,
        r#"╔═╤═╤═╤╤╤╗
║#│a│b│││║
╟─┼─┼─┼┼┼╢
║0│1│2│││║
╚═╧═╧═╧╧╧╝"#,
    );
}

#[test]
fn columns_overflow() {
    check(
        5,
        r#"[{a: 1, b: 2, c: 3, d: 4, e: 5}]"#,
        r#"╔═╤═╗
║#│…║
╟─┼─╢
║0│…║
╚═╧═╝"#,
    );
}

#[test]
fn tiny_terminal() {
    check(
        2,
        r#"[{a: 1}]"#,
        r#"╔═
║…
╟─
║…
╚═"#,
    );
}

#[test]
fn nested_values_in_cells() {
    check(
        80,
        r#"[{a: [1, 2, 3], b: {c: 1}}]"#,
        r#"╔═╤═══════╤════════╗
║#│a      │b       ║
╟─┼───────┼────────╢
║0│1, 2, 3│{ c: 1 }║
╚═╧═══════╧════════╝"#,
    );
}

#[test]
fn odd_keys() {
    check(
        80,
        r#"{ "a b": 1, "": 2 }"#,
        r#"╔═══╤═╗
║a b│1║
║   │2║
╚═══╧═╝"#,
    );
}

#[test]
fn list_of_list_of_objects() {
    check(
        80,
        r#"[[{a: 1}], [2]]"#,
        r#"╔═╤════════╗
║#│0       ║
╟─┼────────╢
║0│{ a: 1 }║
║1│2       ║
╚═╧════════╝"#,
    );
}

#[test]
fn function_help() {
    check(
        80,
        r#"maths.log"#,
        r#"FUNCTION
    maths.log - Calculate the logarithm of a given value

CALLING SYNTAX
    log <n> [<base>]

PARAMETERS
    n - Number to take the logarithm of

    base [optional] - base"#,
    );
}

#[test]
fn class_help() {
    check(
        80,
        r#"maths.Stats"#,
        r#"CLASS
    maths.Stats - Statistics for a data set

PARENT
    core.Object

FIELDS
    min - Smallest value in data set
    max - Largest value in data set
    mean - Average value in the data set
    standardDeviation - Standard deviation
    median - Median value of the data set
    count - Number of elements in the data set

STATIC METHODS
    new - Construct a new Stats object"#,
    );
}

#[test]
fn bound_method_help() {
    check(
        80,
        r#""foo".startsWith"#,
        r#"METHOD
    startsWith - Check if this string starts with another

CLASS
    core.String

CALLING SYNTAX
    string.startsWith <prefix>

PARAMETERS
    prefix - Prefix to test"#,
    );
}

#[test]
fn field_help() {
    check(
        80,
        r#"maths.Stats.helpForField "min""#,
        r#"FIELD
    min - Smallest value in data set

CLASS
    maths.Stats

TYPE
    core.Number"#,
    );
}

/// Scala also lists `open`, which needs the OS (M5)
#[test]
fn tag_class_help() {
    check(
        80,
        r#"net.Url"#,
        r#"CLASS
    net.Url - Tag class for a URL

PARENT
    core.Any

METHODS
    host - Return the host component of this URL (may be null)
    withQueryParams - Add query parameters to this URL"#,
    );
}

#[test]
fn view_class_help() {
    check(
        80,
        r#"view.View"#,
        r#"CLASS
    view.View - Instructions on how to display data in Mash's output system

PARENT
    core.Object

FIELDS
    data - Data to display
    disableCustomViews - If true, disable custom views for this data
    useBrowser - If true, always use the object browser where possible
    useTree - If true, always use the tree object browser where possible

STATIC METHODS
    new - Construct a new View object"#,
    );
}

#[test]
fn method_help_with_description() {
    check(
        80,
        r#"[1,2].sortBy"#,
        r#"METHOD
    sortBy - Sort the elements of a sequence by one or more attributes

CLASS
    collections.List

CALLING SYNTAX
    list.sortBy (--descending | -d) (--naturalOrder | -n) <attributes>+...

PARAMETERS
    --descending | -d [optional] - Sort results from highest value to lowest

    --naturalOrder | -n [optional] - Use natural sort order ('alphanum') when comparing items

    attributes [variadic, at least one] - One or more functions to apply to elements to extract a value to compare
        If multiple attribute functions are supplied, elements are compared by the first attribute.
          If they are equal, then the second attribute is used to order them, and so on.

DESCRIPTION
    Examples:
      ["aa", "b", "ccc"] sortBy length                # ["b", "aa", "ccc"]
      ["aa", "b", "ccc"] | sortBy length --descending # ["ccc", "aa", "b"]
      ["az", "b", "aa"] | sortBy first last           # ["aa", "az", "b"]"#,
    );
}

#[test]
fn lambda_help() {
    check(
        80,
        r#"x => x + 1"#,
        r#"FUNCTION
    anonymous - anonymous function

CALLING SYNTAX
    anonymous <x>

PARAMETERS
    x"#,
    );
}

#[test]
fn user_class_help() {
    check(
        80,
        r#"class Point x y { def norm = x }; Point"#,
        r#"CLASS
    Point

PARENT
    core.Object

FIELDS
    x
    y

STATIC METHODS
    new - Construct a new Point object

METHODS
    norm"#,
    );
}

#[test]
fn user_method_help() {
    check(
        80,
        r#"class Point x y { def norm = x }; Point.helpForMethod "norm""#,
        r#"METHOD
    norm

CLASS
    Point

SOURCE
    def norm = x"#,
    );
}

#[test]
fn user_constructor_help() {
    check(
        80,
        r#"class Point x y; Point.new"#,
        r#"FUNCTION
    new - Construct a new Point object

CALLING SYNTAX
    new <x> <y>

PARAMETERS
    x

    y"#,
    );
}

#[test]
fn user_class_instance() {
    check(
        80,
        r#"class Point x y; Point 1 2"#,
        r#"╔═════╗
║Point║
╟─┬───╢
║x│1  ║
║y│2  ║
╚═╧═══╝"#,
    );
}

#[test]
fn user_class_instances() {
    check(
        80,
        r#"class Point x y; [Point 1 2, Point 3 4]"#,
        r#"╔═╤═╤═╗
║#│x│y║
╟─┼─┼─╢
║0│1│2║
║1│3│4║
╚═╧═╧═╝"#,
    );
}

#[test]
fn user_function_help() {
    check(
        80,
        r#"# Square it
#
# Longer description <mash>square 3</mash> here
# @param n The number
def square (@flag n = 3) xs... = {
    n * n
  }
help square"#,
        r#"FUNCTION
    square - Square it

CALLING SYNTAX
    square --n[=<value>] <xs>...

PARAMETERS
    --n [optional] - The number

    xs [variadic]

DESCRIPTION
    
    Longer description square 3 here

SOURCE
    def square (@flag n = 3) xs... = {
        n * n
      }"#,
    );
}

#[test]
fn user_field_help() {
    check(
        80,
        r#"# A point
# @param x The x
class Point x y { def norm = x }; Point.helpForField "x""#,
        r#"FIELD
    x - The x

CLASS
    Point"#,
    );
}

#[test]
fn strings_with_newlines_in_text_lines() {
    check(80, "[\"a\nb\", \"c\"]", "a\nb\nc");
}

#[test]
fn string_with_newlines() {
    check(80, "\"a\nb\"", "a\nb");
}

#[test]
fn newlines_in_cells_become_spaces() {
    check(
        80,
        "{a: \"line1\nline2\"}",
        r#"╔═╤═══════════╗
║a│line1 line2║
╚═╧═══════════╝"#,
    );
}

#[test]
fn raw_help_object() {
    check(
        80,
        r#"view.raw (maths.Stats.helpForField "min")"#,
        r#"╔═══════════════════════╗
║       FieldHelp       ║
╟───────────┬───────────╢
║name       │min        ║
║owningClass│maths.Stats║
╚═══════════╧═══════════╝"#,
    );
}

#[test]
fn raw_class() {
    check(80, "view.raw maths.Stats", "maths.Stats");
}

#[test]
fn indented_user_method_source() {
    check(
        80,
        "class Point x y {\n  # Norm\n  def norm =\n    x +\n      y\n}\nPoint.helpForMethod \"norm\"",
        r#"METHOD
    norm - Norm

CLASS
    Point

SOURCE
    def norm =
      x +
        y"#,
    );
}

// ------------------------------------------------------------------------------------------------ Not from Scala

/// The object browser arrives in M5; until then, browsing prints (Scala opens the browser)
#[test]
fn browse_prints_for_now() {
    check(80, "view.browse [1]", "[1]");
}

/// Scala fails here: the constructor doesn't set `print`, which the viewer insists on
#[test]
fn view_constructor() {
    check(80, "view.View.new {a: 1} false false true", "── a: 1");
}

/// Scala Mash means not to print a list of units, but its check never matches (it prints `[, ]`)
#[test]
fn list_of_units_prints_nothing() {
    check(80, "[(), ()]", "");
}

/// Scala measures cells in UTF-16 units, misaligning wide characters
#[test]
fn wide_characters_are_measured_in_columns() {
    check(
        80,
        r#"[{a: "日本語"}, {a: "x"}]"#,
        r#"╔═╤══════╗
║#│a     ║
╟─┼──────╢
║0│日本語║
║1│x     ║
╚═╧══════╝"#,
    );
    check(
        8,
        r#"{a: "日本語"}"#,
        r#"╔═╤════╗
║a│日… ║
╚═╧════╝"#,
    );
}

/// Scala would overflow the stack
#[test]
fn cyclic_values_in_trees() {
    check(
        80,
        "o = {a: 1}; o.self = o; view.tree o",
        r#"┌─ a: 1
└─ self: { a: 1, self: {…} }"#,
    );
}

#[test]
fn fuzzy_times() {
    // The test platform's clock says 2026-10-09T12:00:00Z
    check(
        80,
        "[{when: now, n: 1}, {when: now - 3.days, n: 2}]",
        r#"╔═╤═══════════╤═╗
║#│when       │n║
╟─┼───────────┼─╢
║0│moments ago│1║
║1│3 days ago │2║
╚═╧═══════════╧═╝"#,
    );
}

#[test]
fn absolute_times() {
    let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
    let value = ctx.run("test", "{when: now - 1.hours}").unwrap();
    let config = ViewConfig { fuzzy_time: false };
    let lines = view_value(&mut ctx, &value, Dimensions::new(24, 80), &config).unwrap();
    assert_eq!(lines[1].forget_styling(), "║when│2026-10-09 11:00:00║");
}

#[test]
fn dates() {
    check(80, "now.date", "2026-10-09");
}

#[test]
fn table_styles() {
    let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
    let value = ctx.run("test", "[{a: 1}]").unwrap();
    let lines = view_value(&mut ctx, &value, Dimensions::new(24, 80), &ViewConfig::default()).unwrap();
    let label = Style::fg(default_colours::YELLOW);
    // Borders are plain; headers and row labels are yellow; cells are plain
    assert!(lines[0].chars.iter().all(|c| c.style == Style::DEFAULT));
    assert_eq!(
        styles(&lines[1]),
        [Style::DEFAULT, label, Style::DEFAULT, label, Style::DEFAULT]
    );
    assert_eq!(
        styles(&lines[3]),
        [Style::DEFAULT, label, Style::DEFAULT, Style::DEFAULT, Style::DEFAULT]
    );
}

#[test]
fn object_styles() {
    let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
    let value = ctx.run("test", "maths.stats [1]").unwrap();
    let lines = view_value(&mut ctx, &value, Dimensions::new(24, 80), &ViewConfig::default()).unwrap();
    let yellow = Style::fg(default_colours::YELLOW);
    assert!(
        lines[1].chars[1..lines[1].len() - 1]
            .iter()
            .all(|c| c.style == yellow.with_bold())
    );
    assert_eq!(lines[3].chars[1].style, yellow);
    assert_eq!(lines[3].chars[lines[3].len() - 2].style, Style::DEFAULT);
}

fn styles(line: &StyledString) -> Vec<Style> {
    line.chars.iter().map(|c| c.style).collect()
}

/// Scala prints infinity as `∞`
#[test]
fn infinite_numbers() {
    check(
        80,
        "{a: 1/0, b: 0/0}",
        r#"╔═╤════════╗
║a│Infinity║
║b│NaN     ║
╚═╧════════╝"#,
    );
}

#[test]
fn permissions_are_shown_as_rwx() {
    use mash_core::platform::MemoryFiles;
    let files = MemoryFiles::of(&["/home/alice/"])
        .with_file("/home/alice/a.sh", "echo hi")
        .with_permissions("/home/alice/a.sh", 0o750);
    let mut ctx = Ctx::new(Rc::new(TestPlatform {
        files,
        ..TestPlatform::default()
    }));
    let mut show = |source: &str| {
        let value = ctx.run("test", source).unwrap_or_else(|e| panic!("{source}: {e}"));
        let lines = view_value(&mut ctx, &value, Dimensions::new(1000, 80), &ViewConfig::default()).unwrap();
        lines.iter().map(|l| l.forget_styling()).collect::<Vec<_>>().join("\n")
    };
    // As Scala Mash shows them (checked with its `Viewer`)
    assert_eq!(
        show("ls.first.permissions"),
        "╔══════╤═══════╤════════╤══════════╗
║#     │canRead│canWrite│canExecute║
╟──────┼───────┼────────┼──────────╢
║owner │✓      │✓       │✓         ║
║group │✓      │✗       │✓         ║
║others│✗      │✗       │✗         ║
╚══════╧═══════╧════════╧══════════╝"
    );
    assert!(
        show("ls.select 'name' 'permissions'").contains("║0│a.sh│rwxr-x---  ║"),
        "{}",
        show("ls.select 'name' 'permissions'")
    );
    // A section in a cell is `rwx`-style (alone, it's an object table, as in Scala)
    assert_eq!(
        show("{ g: ls.first.permissions.group, n: 1 }"),
        "╔═╤═══╗\n║g│r-x║\n║n│1  ║\n╚═╧═══╝"
    );
}
