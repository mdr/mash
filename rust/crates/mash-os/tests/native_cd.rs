//! `cd` with the native platform. In its own test binary because it changes the process's working directory.

use std::rc::Rc;

use mash_core::eval::{Ctx, render, with_big_stack};
use mash_os::NativePlatform;

#[test]
fn cd_back_and_forward() {
    let result = with_big_stack(|| {
        let dir = tempfile::tempdir().unwrap();
        let real = std::fs::canonicalize(dir.path()).unwrap();
        std::fs::create_dir(real.join("sub")).unwrap();
        let mut ctx = Ctx::new(Rc::new(NativePlatform));
        let code = format!(
            r#"cd "{}"; cd "sub"; a = pwd.name; back; b = pwd; forward; [a, b == "{}", pwd.name, !{{pwd}}.toPath.name]"#,
            real.display(),
            real.display()
        );
        render(&ctx.run("test", &code).unwrap())
    });
    assert_eq!(result, "[sub, true, sub, sub]");
}
