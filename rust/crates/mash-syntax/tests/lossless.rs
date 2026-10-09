//! The tree must always reproduce the input exactly, and parsing must never panic or hang,
//! for every corpus input, every prefix of it, and random single-character deletions.

mod common;

use mash_syntax::{Mode, parse};

fn check(s: &str) {
    for mode in [Mode::Mash, Mode::Mish] {
        let p = parse(s, mode);
        let text = p.syntax().text().to_string();
        assert_eq!(text, s, "not lossless for {s:?} ({mode:?})");
        assert_eq!(u32::from(p.syntax().text_range().len()) as usize, s.len());
    }
}

fn all_inputs() -> Vec<String> {
    common::CORPUS
        .iter()
        .chain(common::BROKEN)
        .map(|s| s.to_string())
        .collect()
}

#[test]
fn corpus_is_lossless() {
    for s in all_inputs() {
        check(&s);
    }
}

#[test]
fn every_prefix_is_lossless() {
    for s in all_inputs() {
        for (i, _) in s.char_indices() {
            check(&s[..i]);
        }
    }
}

#[test]
fn random_deletions_are_lossless() {
    // Tiny LCG so the test is deterministic without a rand dependency
    let mut seed: u64 = 0x2545F4914F6CDD1D;
    let mut next = || {
        seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (seed >> 33) as usize
    };
    for s in all_inputs() {
        let chars: Vec<char> = s.chars().collect();
        if chars.is_empty() {
            continue;
        }
        for _ in 0..30 {
            let mut c = chars.clone();
            for _ in 0..(1 + next() % 3) {
                if !c.is_empty() {
                    let i = next() % c.len();
                    c.remove(i);
                }
            }
            check(&c.into_iter().collect::<String>());
        }
    }
}

#[test]
fn concatenated_corpus_is_lossless() {
    let all = all_inputs().join("\n");
    check(&all);
}

#[test]
fn corpus_parses_without_errors() {
    for s in common::CORPUS {
        let p = parse(s, Mode::Mash);
        assert!(
            p.ok(),
            "unexpected errors for {s:?}: {:?}\n{}",
            p.errors(),
            p.debug_tree()
        );
    }
}

#[test]
fn broken_inputs_report_errors() {
    for s in common::BROKEN {
        let p = parse(s, Mode::Mash);
        assert!(!p.ok(), "expected errors for {s:?}\n{}", p.debug_tree());
    }
}
