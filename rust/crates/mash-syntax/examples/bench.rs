//! Rough parse throughput: `cargo run --release -p mash-syntax --example bench`

use mash_syntax::{Mode, lexer, parse};
use std::time::Instant;

const LINES: &[&str] = &[
    "def square n = n * n",
    "xs = [1, 2, 3, 4, 5] | where (_ >= 3) | map (x => x * 2)",
    "result = { name: \"Hello $user.name!\", size: (ls | sumBy _.size), ok: true }",
    "if a.b?.c > 10 and f(x, y) then g --flag --opt=3 -abc else !{ls -l $dir > out.txt}",
    "obj = { a: { b: [1, 2, { c }] } }; obj.a.b[2].c",
    "class Point x y { def add other = Point (x + other.x) (y + other.y) }",
    "ls",
    "  .permissions",
    "  .owner",
    "\"${ !{git log --oneline $branch} }\" | lines | first 10",
];

fn main() {
    let mut text = String::new();
    let mut n = 0;
    while n < 10_000 {
        for line in LINES {
            text.push_str(line);
            text.push('\n');
            n += 1;
        }
    }
    println!("input: {} lines, {} bytes", n, text.len());

    for _ in 0..3 {
        let start = Instant::now();
        let lexed = lexer::tokenise(&text, false);
        let lex_time = start.elapsed();

        let start = Instant::now();
        let p = parse(&text, Mode::Mash);
        let parse_time = start.elapsed();

        let start = Instant::now();
        let root = p.syntax();
        let nodes = root.descendants().count();
        let walk_time = start.elapsed();

        println!(
            "lex: {:?} ({} tokens) | lex+parse+build: {:?} ({} errors) | walk {} nodes: {:?}",
            lex_time,
            lexed.tokens.len(),
            parse_time,
            p.errors().len(),
            nodes,
            walk_time
        );
    }

    // Pathological nesting: the lookahead scans are O(n) per bracket level, so O(n * depth) overall
    for depth in [100, 1000, 3000] {
        for (name, open, close) in [("parens", "(", ")"), ("blocks", "{ ", "} "), ("lists", "[", "]")] {
            let s = open.repeat(depth) + "x" + &close.repeat(depth);
            let start = Instant::now();
            let p = parse(&s, Mode::Mash);
            println!(
                "nested {name} x{depth}: {:?} ({} errors)",
                start.elapsed(),
                p.errors().len()
            );
        }
    }

    // A typical REPL line, parsed on every keystroke
    let line = "ls | where (_.owner == 'root' and _.size > 1000) | sortBy size --descending | first 10";
    let iterations = 10_000;
    let start = Instant::now();
    for _ in 0..iterations {
        std::hint::black_box(parse(line, Mode::Mash));
    }
    println!("REPL-sized line: {:?} per parse", start.elapsed() / iterations);
}
