//! Ports of `view/render/AssistanceRendererTest.scala`, `IncrementalHistorySearchRendererTest.scala` and
//! `CallingSyntaxRendererTest.scala`.

mod common;

use std::rc::Rc;

use common::LARGE;
use jiff::SignedDuration;
use mash_core::complete::Assistable;
use mash_core::eval::Ctx;
use mash_core::function::{DefaultValue, Param, Params};
use mash_core::platform::{Platform, TestPlatform};
use mash_core::value::Value;
use mash_repl::render::assistance::{render_assistance, render_params};
use mash_repl::render::history_search::render_history_search;
use mash_repl::screen::{Dimensions, Line, Point};
use mash_repl::state::{HistorySearchState, Hit, HitStatus};

fn texts(lines: &[Line]) -> Vec<String> {
    lines.iter().map(|l| l.string.to_string()).collect()
}

fn reverse() -> Assistable {
    let ctx = Ctx::new(Rc::new(TestPlatform::default()));
    match ctx.globals().get_str("reverse") {
        Some(Value::Builtin(b)) => Assistable::Function(b.0),
        other => panic!("reverse is {other:?}"),
    }
}

#[test]
fn assistance_as_large_as_needed() {
    assert_eq!(
        texts(&render_assistance(&reverse(), LARGE)),
        [
            "┌─ reverse ────────────────────────┐",
            "│ Reverse a List, String or Object │",
            "│ reverse <sequence>               │",
            "└──────────────────────────────────┘",
        ]
    );
}

#[test]
fn assistance_truncated_if_there_is_not_enough_space() {
    assert_eq!(
        texts(&render_assistance(&reverse(), Dimensions::new(1000, 10))),
        ["┌─ rev… ─┐", "│ Rever… │", "│ rever… │", "└────────┘"]
    );
}

#[test]
fn assistance_down_to_a_minimum_size() {
    assert_eq!(
        texts(&render_assistance(&reverse(), Dimensions::new(1000, 0))),
        ["┌─  ─┐", "│ R… │", "│ r… │", "└────┘"]
    );
}

const HINTS: &str = "↑ next, ↩ done, ⌃g quit, ⌃d cd, ⌥d this dir only";

#[test]
fn history_search_before_first_hit() {
    let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
    let rendered = render_history_search(&mut ctx, &HistorySearchState::new(""));
    let expected_line = "Incremental history search: ";
    assert_eq!(texts(&rendered.lines), [expected_line, HINTS]);
    assert_eq!(rendered.cursor, Some(Point::new(0, expected_line.len())));
}

#[test]
fn history_search_with_a_hit() {
    let platform = Rc::new(TestPlatform::default());
    let mut ctx = Ctx::new(platform.clone());
    let one_day_ago = platform.now() - SignedDuration::from_hours(24);
    let state = HistorySearchState {
        search: "searchString".into(),
        hit: HitStatus::Hit(Hit {
            result_index: 0,
            match_range: 0..1,
            timestamp: one_day_ago,
            working_directory: "/etc".into(),
        }),
        ..HistorySearchState::new("")
    };
    let rendered = render_history_search(&mut ctx, &state);
    let expected_line = "Incremental history search: searchString";
    assert_eq!(
        texts(&rendered.lines),
        [expected_line, "Hit 1: 1 day ago in /etc", HINTS]
    );
    assert_eq!(rendered.cursor, Some(Point::new(0, expected_line.len())));
}

fn param(name: &'static str, f: impl FnOnce(&mut Param)) -> Param {
    let mut p = Param::named(name);
    f(&mut p);
    p
}

fn syntax(params: Vec<Param>) -> String {
    render_params(&Params::new(params)).to_string()
}

#[test]
fn calling_syntax_of_positional_parameters() {
    let params = vec![
        param("positional", |_| {}),
        param("optional", |p| p.default = Some(DefaultValue::NoArg)),
        param("variadic", |p| p.variadic = true),
    ];
    assert_eq!(syntax(params), "<positional> [<optional>] <variadic>...");
}

#[test]
fn calling_syntax_of_variadic_parameters_needing_one_argument() {
    let params = vec![param("variadicAtLeastOne", |p| {
        p.variadic = true;
        p.variadic_at_least_one = true;
    })];
    assert_eq!(syntax(params), "<variadicAtLeastOne>+...");
}

#[test]
fn calling_syntax_of_flags() {
    let params = vec![
        param("flag", |p| p.flag = true),
        param("flagWithShortName", |p| {
            p.flag = true;
            p.boolean_flag = true;
            p.short_flag = Some('f');
        }),
        param("flagWithCustomValueName", |p| {
            p.flag = true;
            p.flag_value_name = Some("name".into());
        }),
        param("mandatoryValueFlag", |p| {
            p.flag = true;
            p.flag_value_mandatory = true;
        }),
        param("booleanFlag", |p| {
            p.flag = true;
            p.boolean_flag = true;
        }),
    ];
    assert_eq!(
        syntax(params),
        "--flag[=<value>] (--flagWithShortName | -f) --flagWithCustomValueName[=<name>] \
         --mandatoryValueFlag=<value> --booleanFlag"
    );
}
