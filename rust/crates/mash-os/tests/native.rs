//! The native platform against the real OS: processes and the file system, in temporary directories only.
//! (Paths are absolute: tests run in parallel and must not depend on the working directory.)

use std::rc::Rc;

use mash_core::eval::{Ctx, with_big_stack};
use mash_core::platform::{Platform, ProcessInput, ProcessOutput, ProcessRequest};
use mash_core::value::Value;
use mash_os::NativePlatform;

fn env() -> Vec<(String, String)> {
    vec![("PATH".into(), std::env::var("PATH").unwrap_or_default())]
}

fn run(argv: &[&str], stdin: ProcessInput<'_>, stdout: ProcessOutput<'_>) -> mash_core::platform::ProcessOutcome {
    let argv: Vec<String> = argv.iter().map(|s| s.to_string()).collect();
    let env = env();
    NativePlatform
        .run_process(&ProcessRequest {
            argv: &argv,
            env: &env,
            stdin,
            stdout,
        })
        .expect("process runs")
}

#[test]
fn captures_output() {
    let outcome = run(
        &["echo", "hello", "world"],
        ProcessInput::Inherit,
        ProcessOutput::Capture,
    );
    assert_eq!(outcome.stdout, "hello world\n");
    assert_eq!(outcome.exit_status, 0);
    assert!(!outcome.interrupted);
    assert!(outcome.finished >= outcome.started);
}

#[test]
fn reports_exit_status() {
    let outcome = run(&["sh", "-c", "exit 3"], ProcessInput::Inherit, ProcessOutput::Capture);
    assert_eq!(outcome.exit_status, 3);
}

#[test]
fn killed_by_sigint_is_interrupted() {
    let outcome = run(
        &["sh", "-c", "kill -INT $$"],
        ProcessInput::Inherit,
        ProcessOutput::Capture,
    );
    assert!(outcome.interrupted);
    assert_eq!(outcome.exit_status, 128 + 2);
}

#[test]
fn sends_text_to_stdin() {
    let outcome = run(&["cat"], ProcessInput::Text("some\ntext"), ProcessOutput::Capture);
    assert_eq!(outcome.stdout, "some\ntext");
}

#[test]
fn large_input_and_output_do_not_deadlock() {
    // Far bigger than a pipe buffer in both directions
    let text = "x".repeat(4 * 1024 * 1024);
    let outcome = run(&["cat"], ProcessInput::Text(&text), ProcessOutput::Capture);
    assert_eq!(outcome.stdout.len(), text.len());
}

#[test]
fn passes_the_given_environment_only() {
    let argv: Vec<String> = ["sh", "-c", "echo \"$MASH_TEST_VAR:$HOME\""].map(String::from).to_vec();
    let env = vec![
        ("PATH".to_string(), std::env::var("PATH").unwrap_or_default()),
        ("MASH_TEST_VAR".to_string(), "value".to_string()),
    ];
    let outcome = NativePlatform
        .run_process(&ProcessRequest {
            argv: &argv,
            env: &env,
            stdin: ProcessInput::Inherit,
            stdout: ProcessOutput::Capture,
        })
        .unwrap();
    assert_eq!(outcome.stdout, "value:\n");
}

#[test]
fn redirects_to_and_from_files() {
    let dir = tempfile::tempdir().unwrap();
    let input = dir.path().join("in.txt");
    let output = dir.path().join("out.txt");
    std::fs::write(&input, "from a file\n").unwrap();
    let outcome = run(
        &["cat"],
        ProcessInput::File(input.to_str().unwrap()),
        ProcessOutput::File(output.to_str().unwrap()),
    );
    assert_eq!(outcome.stdout, "");
    assert_eq!(std::fs::read_to_string(&output).unwrap(), "from a file\n");
}

#[test]
fn missing_program_is_an_error() {
    let argv = vec!["mash-no-such-program".to_string()];
    let result = NativePlatform.run_process(&ProcessRequest {
        argv: &argv,
        env: &env(),
        stdin: ProcessInput::Inherit,
        stdout: ProcessOutput::Capture,
    });
    assert_eq!(result.unwrap_err().kind(), std::io::ErrorKind::NotFound);
}

/// Evaluate Mash code with the native platform, with `$dir` replaced by a temporary directory
fn eval_in_temp_dir(code: &'static str, setup: fn(&std::path::Path)) -> String {
    with_big_stack(move || {
        let dir = tempfile::tempdir().unwrap();
        setup(dir.path());
        let code = code.replace("$dir", dir.path().to_str().unwrap());
        let mut ctx = Ctx::new(Rc::new(NativePlatform));
        match ctx.run("test", &code) {
            Ok(Value::Str(s)) => s.as_str().to_string(),
            Ok(other) => mash_core::eval::render(&other),
            Err(e) => format!("error: {e}"),
        }
    })
}

#[test]
fn lists_files() {
    let result = eval_in_temp_dir(r#"ls "$dir" | map (.name) | join ",""#, |dir| {
        std::fs::write(dir.join("b.txt"), "bb").unwrap();
        std::fs::write(dir.join(".hidden"), "").unwrap();
        std::fs::create_dir(dir.join("a")).unwrap();
    });
    assert_eq!(result, "a,b.txt");
}

#[test]
fn file_metadata() {
    let result = eval_in_temp_dir(
        r#"s = "$dir/f".info; [s.type, s.size, s.permissions.owner.canRead, s.owner == "$dir".owner].toString"#,
        |dir| std::fs::write(dir.join("f"), "12345").unwrap(),
    );
    assert_eq!(result, "[file, 5, true, true]");
}

#[test]
fn writes_reads_copies_moves_and_deletes() {
    let result = eval_in_temp_dir(
        r#"
        write "$dir/a.txt" ["one", "two"]
        write --append "$dir/a.txt" "three"
        mkdir -c "$dir/x/y"
        cp "$dir/a.txt" "$dir/x/y"
        mv "$dir/x/y/a.txt" "$dir/b.txt"
        rm "$dir/x"
        [readLines "$dir/b.txt", ls "$dir" | map (.name)].toString
        "#,
        |_| {},
    );
    assert_eq!(result, "[[one, two, three], [a.txt, b.txt]]");
}

#[test]
fn symbolic_links() {
    let result = eval_in_temp_dir(
        r#"["$dir/link".info.type, "$dir/link".followLink.name, "$dir/link".isDirectory].toString"#,
        |dir| {
            std::fs::create_dir(dir.join("target")).unwrap();
            std::os::unix::fs::symlink(dir.join("target"), dir.join("link")).unwrap();
        },
    );
    assert_eq!(result, "[link, target, true]");
}

#[test]
fn copying_keeps_symbolic_links_as_links() {
    // As `cp -R` (Scala followed them: a link to a directory above was copied until paths were too long)
    let result = eval_in_temp_dir(
        r#"cp "$dir/d" "$dir/copy"; ["$dir/copy/up".info.type, "$dir/copy/up".followLink == "$dir/d/up".followLink, "$dir/copy/f".info.type].toString"#,
        |dir| {
            std::fs::create_dir(dir.join("d")).unwrap();
            std::fs::write(dir.join("d/f"), "f").unwrap();
            std::os::unix::fs::symlink("..", dir.join("d/up")).unwrap();
        },
    );
    assert_eq!(result, "[link, true, file]");
}

#[test]
fn renaming_can_change_only_case() {
    // On a case-insensitive file system (as macOS's default), "G" exists because "g" does
    let result = eval_in_temp_dir(r#""$dir/g".renameTo "G"; ls "$dir" | map (.name) | join ",""#, |dir| {
        std::fs::write(dir.join("g"), "g").unwrap();
    });
    assert_eq!(result, "G");
    let result = eval_in_temp_dir(r#""$dir/g".renameTo "h""#, |dir| {
        std::fs::write(dir.join("g"), "g").unwrap();
        std::fs::write(dir.join("h"), "h").unwrap();
    });
    assert!(result.ends_with("already exists"), "{result}");
}

#[test]
fn copying_a_file_onto_itself_leaves_it_alone() {
    // Copying a file onto itself once emptied it
    let result = eval_in_temp_dir(
        r#"[try (cp "$dir/f" "$dir") "failed", "$dir/f".read]"#,
        |dir| std::fs::write(dir.join("f"), "f").unwrap(),
    );
    assert_eq!(result, "[failed, f]");
}

#[test]
fn path_run_passes_each_argument_as_it_is() {
    let result = eval_in_temp_dir(r#"("$dir/args.sh".run "two words").stdout"#, |dir| {
        let script = dir.join("args.sh");
        std::fs::write(&script, "#!/bin/sh\necho $#\n").unwrap();
        std::fs::set_permissions(&script, std::os::unix::fs::PermissionsExt::from_mode(0o755)).unwrap();
    });
    assert_eq!(result, "1\n");
}

#[test]
fn recursive_listing_survives_link_cycles() {
    let result = eval_in_temp_dir(r#"ls -r "$dir" | map (.name) | join ",""#, |dir| {
        std::fs::create_dir(dir.join("d")).unwrap();
        std::os::unix::fs::symlink(dir, dir.join("d").join("loop")).unwrap();
    });
    assert_eq!(result, "d,loop");
}

#[test]
fn mish_runs_commands() {
    let result = eval_in_temp_dir(r#"x = 'two words'; !{echo $x ${1 + 2}}.stdout"#, |_| {});
    assert_eq!(result, "two words 3\n");
}

#[test]
fn glob_finds_files() {
    let result = eval_in_temp_dir(r#"glob "$dir/**/*.rs" | map (.name) | join ",""#, |dir| {
        std::fs::create_dir_all(dir.join("src/nested")).unwrap();
        std::fs::write(dir.join("top.rs"), "").unwrap();
        std::fs::write(dir.join("src/nested/deep.rs"), "").unwrap();
        std::fs::write(dir.join("src/other.txt"), "").unwrap();
    });
    assert_eq!(result, "top.rs,deep.rs");
}
