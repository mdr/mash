//! The browser build's stack: wasm has no guard pages or growable stacks, so the stack is fixed when the module
//! is linked. Must match `mash_web::platform::WASM_STACK_SIZE`, which Mash checks against so as not to overflow it.

const STACK_SIZE: u32 = 16 << 20;

fn main() {
    if std::env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("wasm32") {
        println!("cargo:rustc-link-arg-cdylib=-zstack-size={STACK_SIZE}");
    }
}
