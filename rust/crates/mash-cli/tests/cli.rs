//! The `mash` binary: evaluating expressions, running scripts, and `-c` via sh.

use std::process::{Command, Output};

fn mash(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_mash"))
        .args(args)
        .output()
        .expect("run mash")
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

#[test]
fn evaluates_an_expression() {
    let output = mash(&["-e", "def f n = if n <= 1 then 1 else n * f (n - 1); f 10"]);
    assert!(output.status.success(), "{}", stderr(&output));
    assert_eq!(stdout(&output), "3628800\n");
}

#[test]
fn reports_errors_with_their_location() {
    let output = mash(&["-e", "1 +\n  nosuch"]);
    assert_eq!(output.status.code(), Some(1));
    assert!(
        stderr(&output).starts_with("error: No binding for 'nosuch'"),
        "{}",
        stderr(&output)
    );
    assert!(stderr(&output).contains("--> <expression>:2:3"), "{}", stderr(&output));
}

#[test]
fn reports_syntax_errors() {
    let output = mash(&["-e", "(1 +"]);
    assert_eq!(output.status.code(), Some(1));
    assert!(stderr(&output).starts_with("syntax error:"), "{}", stderr(&output));
}

#[test]
fn runs_a_script() {
    let dir = std::env::temp_dir().join(format!("mash-cli-test-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let script = dir.join("script.mash");
    std::fs::write(&script, "def double n = n * 2\nx = double 21\nerror \"x is $x\"\n").unwrap();
    let output = mash(&[script.to_str().unwrap()]);
    std::fs::remove_dir_all(&dir).unwrap();
    assert_eq!(output.status.code(), Some(1));
    assert!(stderr(&output).starts_with("error: x is 42"), "{}", stderr(&output));
}

#[test]
fn dash_c_runs_the_command_with_sh() {
    let output = mash(&["-c", "echo hello; exit 3"]);
    assert_eq!(stdout(&output), "hello\n");
    assert_eq!(output.status.code(), Some(3));
}
