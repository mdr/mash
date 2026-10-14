//! What runs before the first prompt (Scala `Loader` and `InitScriptRunner`): library files from
//! `~/.mash/lib`, whose definitions are published under the namespace each declares. (`~/.mash/init.mash` is
//! then run by the REPL, which prints its errors like a command's.) Also `mash.reload`'s part in the shell.

use std::path::{Path, PathBuf};
use std::rc::Rc;

use mash_core::eval::{Ctx, ReloadHook, RunError};
use mash_core::value::{Object, Value};

/// Load every `.mash` file under `lib_dir` (in path order). Each must declare a namespace; its definitions go
/// into `ns` under that namespace, and the top-level namespaces are also made global. Problems are reported
/// on standard error and skipped.
pub(crate) fn load_libraries(ctx: &mut Ctx, lib_dir: &Path) {
    let mut files = vec![];
    find_mash_files(lib_dir, &mut files);
    files.sort();
    let Some(Value::Object(ns)) = ctx.globals().get_str("ns") else {
        return;
    };
    for file in files {
        let name = file.display().to_string();
        let text = match std::fs::read_to_string(&file) {
            Ok(text) => text,
            Err(e) => {
                eprintln!("mash: could not read {name}: {e}");
                continue;
            }
        };
        match ctx.load_library(&name, &text) {
            Ok((Some(namespace), vars)) => {
                for (key, value) in vars.entries() {
                    if let Value::Str(var) = key {
                        populate(&ns, &namespace, var.as_str(), value);
                    }
                }
            }
            // Scala Mash ignores library files without a namespace
            Ok((None, _)) => {}
            Err(error) => report(&name, &text, error),
        }
    }
    for (name, value) in ns.entries() {
        ctx.globals().set(name, value);
    }
}

/// What `mash.reload` does once the standard globals are reset (Scala `ReloadFunction`): load the library
/// files again, then run `init.mash`, reporting its errors (as at startup) rather than failing the reload.
pub(crate) fn reload_hook(mash_dir: PathBuf) -> ReloadHook {
    Rc::new(move |ctx: &mut Ctx| {
        load_libraries(ctx, &mash_dir.join("lib"));
        let init = mash_dir.join("init.mash");
        if let Ok(text) = std::fs::read_to_string(&init) {
            let name = init.display().to_string();
            if let Err(error) = ctx.eval_source(&name, &text) {
                eprintln!("{}", error.report());
            }
        }
        Ok(())
    })
}

/// Set `name` in the object at `path` below `obj`, creating objects along the way
fn populate(obj: &Object, path: &[String], name: &str, value: Value) {
    match path.split_first() {
        None => obj.set_str(name, value),
        Some((first, rest)) => {
            let child = match obj.get_str(first) {
                Some(Value::Object(child)) => child,
                _ => {
                    let child = Object::new();
                    obj.set_str(first, Value::Object(child.clone()));
                    child
                }
            };
            populate(&child, rest, name, value);
        }
    }
}

fn find_mash_files(dir: &Path, files: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            find_mash_files(&path, files);
        } else if path.extension().is_some_and(|e| e == "mash") {
            files.push(path);
        }
    }
}

fn report(name: &str, text: &str, error: RunError) {
    match error {
        RunError::Syntax(errors) => {
            for error in errors {
                let line = text[..usize::from(error.range.start())].matches('\n').count() + 1;
                eprintln!("syntax error: {} ({name}:{line})", error.message);
            }
        }
        RunError::Eval(error) => eprintln!("{}", error.report()),
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use mash_core::eval::with_big_stack;
    use mash_core::platform::TestPlatform;

    use super::*;

    #[test]
    fn library_definitions_are_published_under_their_namespace() {
        let dir = tempfile::tempdir().unwrap();
        let lib = dir.path().join("lib");
        std::fs::create_dir_all(lib.join("sub")).unwrap();
        std::fs::write(
            lib.join("greet.mash"),
            "namespace acme.greetings\ndef hello name = 'Hello, ' + name",
        )
        .unwrap();
        std::fs::write(lib.join("sub/maths.mash"), "namespace acme\ndef double n = n * 2").unwrap();
        std::fs::write(lib.join("ignored.txt"), "namespace nope\ndef x = 1").unwrap();
        let result = with_big_stack(move || {
            let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
            load_libraries(&mut ctx, &lib);
            let value = ctx
                .run(
                    "test",
                    "[acme.greetings.hello 'lib', acme.double 21, ns.acme.greetings.hello 'x', global.get 'nope']",
                )
                .unwrap();
            ctx.stringify(&value).unwrap()
        });
        assert_eq!(result, "[\"Hello, lib\", 42, \"Hello, x\", null]");
    }

    #[test]
    fn reload_resets_builtins_and_reruns_libraries_and_init() {
        let dir = tempfile::tempdir().unwrap();
        let mash_dir = dir.path().to_path_buf();
        std::fs::create_dir_all(mash_dir.join("lib")).unwrap();
        std::fs::write(mash_dir.join("lib/a.mash"), "namespace acme\ndef answer = 42").unwrap();
        std::fs::write(
            mash_dir.join("init.mash"),
            "initRuns = (global.get 'initRuns' or 0) + 1",
        )
        .unwrap();
        let lib_file = mash_dir.join("lib/a.mash");
        let result = with_big_stack(move || {
            let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
            ctx.set_reload_hook(reload_hook(mash_dir));
            ctx.run("test", "def length x = 'mine'; ns.core = 'gone'").unwrap();
            std::fs::write(lib_file, "namespace acme\ndef answer = 43").unwrap();
            ctx.run("test", "mash.reload").unwrap();
            let value = ctx
                .run(
                    "test",
                    "[length [1, 2], acme.answer, ns.acme.answer, initRuns, ns.core.identity 1]",
                )
                .unwrap();
            ctx.stringify(&value).unwrap()
        });
        assert_eq!(result, "[2, 43, 43, 1, 1]");
    }
}
