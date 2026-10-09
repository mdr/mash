//! The parser bounds its recursion (and the depth of the tree it builds) by `MAX_NESTING_DEPTH`, reporting
//! an error instead of overflowing the native stack.

use mash_syntax::{MAX_NESTING_DEPTH, Mode, Parse, parse};

/// Parse on a thread with a deliberately modest stack, so these tests catch growth in per-level stack use.
/// (Real use runs on a big-stack thread; this is a regression guard, not the production configuration.)
fn parse_on_small_stack(text: String) -> (Parse, String) {
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(move || {
            let p = parse(&text, Mode::Mash);
            let round_trip = p.syntax().text().to_string();
            (p, round_trip)
        })
        .unwrap()
        .join()
        .unwrap()
}

fn too_deep_errors(p: &Parse) -> usize {
    p.errors()
        .iter()
        .filter(|e| e.message.contains("too deeply nested"))
        .count()
}

#[test]
fn deep_but_allowed_nesting_parses() {
    let n = 990;
    let text = format!("{}1{}", "(".repeat(n), ")".repeat(n));
    let (p, round_trip) = parse_on_small_stack(text.clone());
    assert!(p.ok(), "{:?}", p.errors());
    assert_eq!(round_trip, text);
}

#[test]
fn excessive_nesting_is_one_error_and_lossless() {
    for (open, close) in [("(", ")"), ("[", "]"), ("{", "}"), ("\"${", "}\"")] {
        let n = MAX_NESTING_DEPTH as usize + 100;
        let text = format!("{}1{}", open.repeat(n), close.repeat(n));
        let (p, round_trip) = parse_on_small_stack(text.clone());
        assert_eq!(round_trip, text, "lossless for {open}");
        assert_eq!(
            too_deep_errors(&p),
            1,
            "{open}: {:?}",
            &p.errors()[..p.errors().len().min(5)]
        );
        assert_eq!(p.errors().len(), 1, "later errors are suppressed for {open}");
    }
}

#[test]
fn very_deep_nesting_does_not_overflow() {
    let n = 200_000;
    let text = format!("{}1{}", "(".repeat(n), ")".repeat(n));
    let (p, round_trip) = parse_on_small_stack(text.clone());
    assert_eq!(round_trip, text);
    assert_eq!(too_deep_errors(&p), 1);
}

#[test]
fn other_recursive_constructs_are_bounded() {
    let n = MAX_NESTING_DEPTH as usize + 100;
    let cases = [
        "- ".repeat(n) + "1",
        "x => ".repeat(n) + "1",
        "1 | x => ".repeat(n) + "1",
        "a or ".repeat(n) + "b",
        "a and ".repeat(n) + "b",
        format!("[{}x{}] = y", "[".repeat(n), "]".repeat(n)),
    ];
    for text in cases {
        let (p, round_trip) = parse_on_small_stack(text.clone());
        assert_eq!(round_trip, text);
        assert_eq!(too_deep_errors(&p), 1, "{}...", &text[..20]);
    }
}

#[test]
fn long_left_deep_chains_are_bounded() {
    let n = MAX_NESTING_DEPTH as usize + 100;
    let cases = [
        format!("1{}", " + 1".repeat(n)),
        format!("1{}", " * 1".repeat(n)),
        format!("a{}", ".b".repeat(n)),
        format!("a{}", "[0]".repeat(n)),
        format!("a{}", " | f".repeat(n)),
        format!("f{}", "(1)".repeat(n)),
    ];
    for text in cases {
        let (p, round_trip) = parse_on_small_stack(text.clone());
        assert_eq!(round_trip, text);
        assert_eq!(too_deep_errors(&p), 1, "{}...", &text[..20]);
    }
}

#[test]
fn moderate_chains_are_fine() {
    let text = format!("1{}", " + 1".repeat(900));
    let (p, _) = parse_on_small_stack(text);
    assert!(p.ok());
}
