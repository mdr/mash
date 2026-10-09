//! Print the syntax tree of each argument: `cargo run -p mash-syntax --example tree -- "a + b"`

fn main() {
    for s in std::env::args().skip(1) {
        let p = mash_syntax::parse(&s, mash_syntax::Mode::Mash);
        println!("{s:?} errors={:?}\n{}", p.errors(), p.debug_tree());
    }
}
