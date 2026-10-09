//! The browser build's stack: wasm has no guard pages or growable stacks, so the stack is fixed when the module
//! is linked. 16 MiB fits the call depth limit (`mash_core::eval::DEFAULT_MAX_DEPTH` on wasm) in a debug build;
//! `www/test/stack.mjs` checks this.

const STACK_SIZE: u32 = 16 << 20;

fn main() {
    if std::env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("wasm32") {
        println!("cargo:rustc-link-arg-cdylib=-zstack-size={STACK_SIZE}");
    }
}
