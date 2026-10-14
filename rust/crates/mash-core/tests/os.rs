//! The os builtins and system commands, against `TestPlatform`'s in-memory files and fake commands. Expected
//! values were checked against Scala Mash where it can run them (it can't run processes without a terminal);
//! comments mark deliberate differences.

use std::rc::Rc;

use mash_core::eval::{Ctx, RunError, render, with_big_stack};
use mash_core::platform::{FakeOutcome, MemoryFiles, TestPlatform};

/// Home `/home/alice` (the working directory) containing `a.txt` ("hi\n"), `.hidden`, `sub/b` (empty), and
/// `/tmp/`
fn files() -> MemoryFiles {
    MemoryFiles::of(&["/home/alice/.hidden", "/home/alice/sub/b", "/tmp/"]).with_file("/home/alice/a.txt", "hi\n")
}

fn platform() -> TestPlatform {
    TestPlatform {
        files: files(),
        ..TestPlatform::default()
    }
}

/// Run code with a platform, returning the rendered result (or the error) and the platform afterwards
fn run_with(platform: TestPlatform, code: &'static str) -> (String, Rc<TestPlatform>) {
    let platform = Rc::new(platform);
    let mut ctx = Ctx::new(platform.clone());
    let result = match ctx.run("test", code) {
        Ok(value) => render(&value),
        Err(RunError::Eval(e)) if e.is_interrupted() => "interrupted".to_string(),
        Err(e) => format!("error: {e}"),
    };
    (result, platform)
}

fn eval(code: &'static str) -> String {
    run_with(platform(), code).0
}

/// Check each `(code, expected rendering)` pair, in a fresh interpreter each
fn check(cases: &'static [(&'static str, &'static str)]) {
    with_big_stack(move || {
        let mut failures = vec![];
        for (code, expected) in cases {
            let actual = eval(code);
            if actual != *expected {
                failures.push(format!("{code:?}: got {actual:?}, expected {expected:?}"));
            }
        }
        assert!(
            failures.is_empty(),
            "{} failing:\n  {}",
            failures.len(),
            failures.join("\n  ")
        );
    })
}

#[test]
fn working_directory() {
    check(&[
        ("pwd", "/home/alice"),
        ("pwd.getClass", "core.String"),
        ("home", "/home/alice"),
        (r#"cd "sub"; pwd"#, "/home/alice/sub"),
        (r#"cd "/tmp"; cd; pwd"#, "/home/alice"),
        (r#"cd "~/sub"; pwd"#, "/home/alice/sub"),
        (r#""sub".cd; pwd"#, "/home/alice/sub"),
        (r#"ls.last.cd; pwd"#, "/home/alice/sub"),
        (
            r#"cd "nonexistent""#,
            "error: Invalid argument 'directory'. Could not change directory to 'nonexistent', not a directory",
        ),
        (
            r#"cd "a.txt""#,
            "error: Invalid argument 'directory'. Could not change directory to 'a.txt', not a directory",
        ),
        ("oldDirs", "[]"),
        (r#"cd "/tmp"; oldDirs"#, "[/home/alice]"),
        (r#"cd "/tmp"; back; pwd"#, "/home/alice"),
        (r#"cd "/tmp"; back; forward; pwd"#, "/tmp"),
        ("back", "error: No previous directory in history"),
        ("forward", "error: No next directory in history"),
        (r#"cd "sub"; up; pwd"#, "/home/alice"),
        ("up 2; pwd", "/"),
        (r#"within "sub" pwd"#, "/home/alice/sub"),
        (r#"within "sub" pwd; pwd"#, "/home/alice"),
        (
            r#"within "nonexistent" pwd"#,
            "error: Could not change directory to 'nonexistent', not a directory",
        ),
    ]);
}

#[test]
fn listing_files() {
    check(&[
        ("ls | map (.path)", "[a.txt, sub]"),
        ("ls -a | map (.path)", "[.hidden, a.txt, sub]"),
        ("ls -r | map (.path)", "[a.txt, sub, sub/b]"),
        (r#"ls "sub" | map (.path)"#, "[sub/b]"),
        (r#"ls "sub" "a.txt" | map (.path)"#, "[sub/b, a.txt]"),
        (r#"ls ["sub", "a.txt"] | map (.path)"#, "[sub/b, a.txt]"),
        (r#"ls -d "sub" | map (.path)"#, "[sub]"),
        (r#"ls "/home" | map (.path)"#, "[/home/alice]"),
        (r#"ls "nope""#, "error: Invalid argument 'paths'. No such file 'nope'"),
        ("ls.first.getClass", "os.PathSummary"),
        (
            "ls.first",
            "{ path: a.txt, type: file, size: 3, owner: alice, group: alice, permissions: { owner: { canRead: \
             true, canWrite: true, canExecute: false }, group: { canRead: true, canWrite: false, canExecute: \
             false }, others: { canRead: true, canWrite: false, canExecute: false } }, lastModified: \
             2026-10-01T09:00:00+00:00 }",
        ),
        ("ls.last.type", "dir"),
        ("ls.first.size.getClass", "core.Number"),
        ("ls.first.size.tag", "core.Bytes"),
        ("ls.first.type.tag", "os.FileType"),
        ("ls.first.owner.tag", "os.Username"),
        ("ls.first.group.tag", "os.Group"),
        ("ls.first.path.tag", "os.Path"),
        ("ls.first.permissions.getClass", "os.Permissions"),
        ("ls.first.permissions.owner.getClass", "os.PermissionsSection"),
        ("children | map (.path)", "[.hidden, a.txt, sub]"),
        ("children -i | map (.path)", "[a.txt, sub]"),
        (r#"children "sub" | map (.path)"#, "[sub/b]"),
        (r#""sub".children | map (.path)"#, "[sub/b]"),
        (
            r#"children "a.txt""#,
            "error: Invalid argument 'directory'. 'a.txt' is not a directory",
        ),
        (r#""nope".children"#, "error: 'nope' does not exist"),
        (r#"glob "*.txt" | map (.path)"#, "[a.txt]"),
        (r#""*.txt".glob | map (.path)"#, "[a.txt]"),
        (r#"glob "*" | map (.path)"#, "[a.txt, sub]"),
        (r#"glob ".*" | map (.path)"#, "[.hidden]"),
        (r#"glob "s*/*" | map (.path)"#, "[sub/b]"),
        (r#"glob "**" | map (.path)"#, "[\"\", a.txt, sub, sub/b]"),
        (r#"glob "/home/*/sub" | map (.path)"#, "[/home/alice/sub]"),
        (r#"glob "{a.txt,sub}" | map (.path)"#, "[a.txt, sub]"),
        (r#"glob "nope*""#, "[]"),
    ]);
}

#[test]
fn path_methods() {
    check(&[
        (r#""a//b/".name"#, "b"),
        (r#""a//b/".parent"#, "a"),
        (r#""a/b.c.d".extension"#, "d"),
        (r#""a/b.c.d".baseName"#, "b.c"),
        (r#""a/b".extension"#, "null"),
        // A leading dot is part of the name (as Python's and Nushell's)
        (r#"".hidden".extension"#, "null"),
        (r#"".hidden".baseName"#, ".hidden"),
        (r#""a/.config.json".extension"#, "json"),
        (r#""a/.config.json".baseName"#, ".config"),
        (r#""..".extension"#, "null"),
        (r#""..".baseName"#, ".."),
        (r#""/a".parent"#, "/"),
        (r#""a".parent"#, "null"),
        (r#""/".parent"#, "null"),
        // Scala: NullPointerException
        (r#""/".name"#, "null"),
        (r#""/a/b".segments"#, "[a, b]"),
        (r#""/a/b".segments.first.tag"#, "os.Path"),
        (r#""a.txt".exists"#, "true"),
        (r#""nope".exists"#, "false"),
        (r#""sub".isDirectory"#, "true"),
        (r#""a.txt".isDirectory"#, "false"),
        (r#""a.txt".isFile"#, "true"),
        (r#""sub".isEmptyDir"#, "false"),
        (r#""/tmp".isEmptyDir"#, "true"),
        (r#""a.txt".size"#, "3"),
        (r#""a.txt".type"#, "file"),
        (r#""a.txt".owner"#, "alice"),
        (r#""a.txt".group"#, "alice"),
        (r#""a.txt".lastModified"#, "2026-10-01T09:00:00+00:00"),
        (r#""a.txt".permissions.owner.canWrite"#, "true"),
        (r#""a.txt".info.path"#, "a.txt"),
        (r#""sub/../a.txt".absolute"#, "/home/alice/a.txt"),
        (
            r#""nope".absolute"#,
            "error: Could not resolve 'nope': No such file or directory: nope",
        ),
        (r#""a.txt".withTilde"#, "a.txt"),
        (r#"home.withTilde"#, "~"),
        (r#""/home/alice/sub".withTilde"#, "~/sub"),
        (r#""/home/alicex".withTilde"#, "/home/alicex"),
        (r#""a.txt".read"#, "hi\n"),
        (r#""a.txt".readLines"#, "[hi]"),
        (r#""sub".followLink"#, "error: Path 'sub' is not a link"),
        (r#""nope".followLink"#, "error: Path 'nope' does not exist"),
        // PathSummary has the Path methods
        ("ls.first.name", "a.txt"),
        ("ls.first.parent", "null"),
        ("ls.first.readLines", "[hi]"),
        ("ls.first.extension", "txt"),
        ("ls.last.children | map (.path)", "[sub/b]"),
    ]);
}

#[test]
fn reading_and_writing() {
    check(&[
        (r#"read "a.txt""#, "hi\n"),
        (r#"readLines "a.txt""#, "[hi]"),
        (r#"cat "a.txt""#, "[hi]"),
        (
            r#"cat "nope""#,
            "error: Could not read 'nope': No such file or directory: nope",
        ),
        (r#"write "new.txt" 42; read "new.txt""#, "42"),
        (r#"write "new.txt" [1, 2]; read "new.txt""#, "1\n2\n"),
        (
            r#"write "a.txt" "more"; write --append "a.txt" "!"; read "a.txt""#,
            "more!",
        ),
        (r#"write -a "a.txt" ["x"]; readLines "a.txt""#, "[hi, x]"),
        (r#""b.txt".write "hello"; "b.txt".read"#, "hello"),
        (r#""b.txt".write ["x", "y"]; "b.txt".readLines"#, "[x, y]"),
        (r#""a.txt".write --append "x"; "a.txt".read"#, "hi\nx"),
        (r#"write "c.txt" "a`r`nb`rc"; readLines "c.txt""#, "[a, b, c]"),
        (r#"f = createTempFile; f.write "x"; [f.read, f.parent]"#, "[x, /tmp]"),
        (r#"d = createTempDirectory; d.isEmptyDir"#, "true"),
    ]);
}

#[test]
fn creating_copying_moving_and_deleting() {
    check(&[
        (r#"mkdir "d"; "d".isDirectory"#, "true"),
        (r#"mkdir "d""#, "d"),
        (r#"mkdir "d" "e""#, "[d, e]"),
        (
            r#"mkdir "x/y""#,
            "error: Could not create directory 'x/y': No such file or directory: x/y",
        ),
        (r#"mkdir -c "x/y"; "x/y".isDirectory"#, "true"),
        // Without a trailing slash, as other paths are shown
        (r#"mkdir "m/""#, "m"),
        (r#"ls -d "sub/" | map (.path)"#, "[sub]"),
        // Up at least one level
        (r#"up 0"#, "error: Invalid argument 'n'. Must be at least 1, but was 0"),
        (r#""x/y".createDirectory -c"#, "x/y"),
        (r#"rm "a.txt"; ls | map (.path)"#, "[sub]"),
        (r#"rm "sub"; ls | map (.path)"#, "[a.txt]"),
        (r#"rm "sub/b" "sub"; ls | map (.path)"#, "[a.txt]"),
        (r#""a.txt".delete; "a.txt".exists"#, "false"),
        (
            r#"rm "nope""#,
            "error: Could not delete 'nope': No such file or directory: nope",
        ),
        // Every path that can be deleted is, then each failure is reported (as `rm`)
        (
            r#"rm "sub" "nope" "a.txt""#,
            "error: Could not delete 'nope': No such file or directory: nope",
        ),
        (r#"try (rm "sub" "nope" "a.txt") 0; ls"#, "[]"),
        (
            r#"rm "x" "a.txt" "nope""#,
            "error: Could not delete 'x': No such file or directory: x\nCould not delete 'nope': No such file or directory: nope",
        ),
        (r#"cp "a.txt" "c.txt"; read "c.txt""#, "hi\n"),
        (r#"cp "a.txt" "sub"; ls "sub" | map (.path)"#, "[sub/a.txt, sub/b]"),
        (r#"cp "sub" "copy"; ls -r "copy" | map (.path)"#, "[copy/b]"),
        (
            r#"cp "sub" "/tmp"; glob "/tmp/**" | map (.path)"#,
            "[/tmp, /tmp/sub, /tmp/sub/b]",
        ),
        // A file is overwritten and a directory merged into (as `cp -R`), but they never replace each other
        (
            r#"cp "sub" "a.txt""#,
            "error: Cannot overwrite the file 'a.txt' with a directory",
        ),
        (r#"write "c.txt" "old"; cp "a.txt" "c.txt"; read "c.txt""#, "hi\n"),
        (
            r#"mkdir -c "copy/sub"; write "copy/sub/c" ""; cp "sub" "copy"; ls -r "copy" | map (.path)"#,
            "[copy/sub, copy/sub/b, copy/sub/c]",
        ),
        (r#"cp "a.txt" "a.txt""#, "error: Cannot copy 'a.txt' onto itself"),
        (
            r#"cp "a.txt" "."; read "a.txt""#,
            "error: Cannot copy 'a.txt' onto itself",
        ),
        // A copy into the directory being copied doesn't copy itself (as `cp -R` and Commons IO)
        (
            r#"cp "sub" "sub/inner"; ls -r "sub" | map (.path)"#,
            "[sub/b, sub/inner, sub/inner/b]",
        ),
        (
            r#"mkdir "sub/d"; cp "sub" "sub/d"; ls -r "sub" | map (.path)"#,
            "[sub/b, sub/d, sub/d/sub, sub/d/sub/b, sub/d/sub/d]",
        ),
        (
            r#"cp "a.txt" "sub/b" "nope""#,
            "error: Multiple source paths were provided, but the destination was not a directory",
        ),
        (r#""a.txt".copy "c.txt"; "c.txt".read"#, "hi\n"),
        (
            r#""a.txt".copy "sub""#,
            "error: Cannot overwrite the directory 'sub' with a file",
        ),
        (r#"write "c.txt" "old"; "a.txt".copy "c.txt"; "c.txt".read"#, "hi\n"),
        (r#"mkdir "c"; "sub".copy "c"; ls "c" | map (.path)"#, "[c/b]"),
        (r#""a.txt".copyInto "sub""#, "sub/a.txt"),
        (
            r#""a.txt".copyInto "a.txt""#,
            "error: Cannot copy into a.txt, not a directory",
        ),
        (r#"mv "a.txt" "c.txt"; ls | map (.path)"#, "[c.txt, sub]"),
        (r#"mv "a.txt" "sub"; ls -r | map (.path)"#, "[sub, sub/a.txt, sub/b]"),
        // The destination is relative to the current directory, as `mv`'s (Scala: the source's directory)
        (r#"mv "sub/b" "c"; [ls "sub" | map (.path), "c".exists]"#, "[[], true]"),
        (
            r#"mv "a.txt" "sub/b" "nope""#,
            "error: Multiple files can only be moved into a directory",
        ),
        (r#""a.txt".moveInto "sub""#, "sub/a.txt"),
        // A file is overwritten, and an empty directory replaced (as `mv`)
        (r#"write "sub/a.txt" "old"; mv "a.txt" "sub"; read "sub/a.txt""#, "hi\n"),
        (
            r#"write "sub/a.txt" "old"; "a.txt".moveInto "sub"; read "sub/a.txt""#,
            "hi\n",
        ),
        (r#"write "c.txt" "old"; mv "a.txt" "c.txt"; read "c.txt""#, "hi\n"),
        (
            r#"mkdir -c "d/sub"; mv "sub" "d"; ls -r "d" | map (.path)"#,
            "[d/sub, d/sub/b]",
        ),
        (
            r#"mkdir -c "d/sub"; write "d/sub/c" ""; mv "sub" "d""#,
            "error: Cannot move 'sub' to 'd/sub': a directory that isn't empty is there",
        ),
        (
            r#"mkdir -c "d/sub"; write "d/sub/c" ""; "sub".moveInto "d""#,
            "error: Cannot move 'sub' to 'd/sub': a directory that isn't empty is there",
        ),
        (
            r#"mkdir -c "d/a.txt"; mv "a.txt" "d""#,
            "error: Cannot overwrite the directory 'd/a.txt' with a file",
        ),
        (r#"mv "a.txt" "a.txt"; read "a.txt""#, "hi\n"),
        (
            r#""a.txt".moveInto "nope""#,
            "error: Invalid argument 'destination'. 'nope' does not exist",
        ),
        (r#""a.txt".renameTo "c.txt"; ls | map (.path)"#, "[c.txt, sub]"),
        (r#""sub/b".renameTo "c""#, "sub/c"),
        (
            r#""a.txt".renameTo "x/y""#,
            "error: Invalid argument 'newName'. Name cannot contain directory separators",
        ),
        (
            r#""a.txt".renameTo """#,
            "error: Invalid argument 'newName'. Name cannot be empty",
        ),
        (
            r#""a.txt".renameTo "..""#,
            "error: Invalid argument 'newName'. Name cannot be . or ..",
        ),
        (r#""a.txt".renameBy (_.toUpper)"#, "A.TXT"),
        (
            r#""a.txt".renameTo "sub""#,
            "error: Could not rename 'a.txt': 'sub' already exists",
        ),
    ]);
}

#[test]
fn permissions() {
    check(&[
        (r#""a.txt".setExecutable.owner.canExecute"#, "true"),
        (
            r#""a.txt".setExecutable; "a.txt".permissions.group.canExecute"#,
            "false",
        ),
        (
            r#""a.txt".setExecutable --all; "a.txt".permissions.others.canExecute"#,
            "true",
        ),
        (r#""a.txt".setExecutable -og | .owner.canExecute"#, "false"),
        (r#""a.txt".setExecutable -og | .group.canExecute"#, "true"),
        (r#""a.txt".setReadable false | .owner.canRead"#, "false"),
        (r#""a.txt".setWritable --group | .group.canWrite"#, "true"),
        (r#""a.txt".setWritable --all false | .owner.canWrite"#, "false"),
    ]);
}

#[test]
fn which() {
    let platform = TestPlatform {
        files: MemoryFiles::of(&["/bin/ls", "/usr/bin/"]).with_permissions("/bin/ls", 0o755),
        env: [("PATH".to_string(), "/usr/bin:/bin".to_string())].into(),
        ..TestPlatform::default()
    };
    let result = with_big_stack(move || run_with(platform, r#"[which "ls", findExecutableInPath "nope"]"#).0);
    assert_eq!(result, "[/bin/ls, null]");
}

// ------------------------------------------------------------------------------------------------ processes

#[test]
fn system_commands() {
    check(&[
        ("!{echo hi}.getClass", "os.ProcessResult"),
        ("!{echo hi}.stdout", "hi\n"),
        ("!{echo hi}.exitStatus", "0"),
        ("!{echo hi}.succeeded", "true"),
        ("!{false}.failed", "true"),
        ("!{false}.exitStatus", "1"),
        ("!{echo a b}.lines", "[a b]"),
        (r#"!{cat} ["a", "b"] | .lines"#, "[a, b]"),
        ("!{echo 42}.toNumber", "42"),
        ("!{echo /tmp}.toPath.tag", "os.Path"),
        ("!{echo hi}.line", "hi"),
        ("!{true}.line", ""),
        ("!{echo hi}.duration", "0"),
        ("!{echo hi}.toList", "[hi]"),
        (r#"x = 3; !{echo $x ${x + 1}}.stdout"#, "3 4\n"),
        (r#"x = [1, 2]; !{echo $x}.stdout"#, "1 2\n"),
        (r#"!{echo "a b" 'c' 1}.stdout"#, "a b c 1\n"),
        (r#"!{cat} "foo" | .stdout"#, "foo"),
        (r#"!{cat} ["a", "b"] | .stdout"#, "a\nb\n"),
        (r#"!{cat "a.txt"}.stdout"#, "hi\n"),
        (r#"!{cat < "a.txt"}.stdout"#, "hi\n"),
        (r#"!{echo hello > "out.txt"}.stdout + read "out.txt""#, "hello\n"),
        ("!!{echo hi}", ""),
        ("!!{echo hi}.getClass", "core.Unit"),
        (r#"!{cd sub}; pwd"#, "/home/alice/sub"),
        (r#"!!{cd /tmp}; !!{cd}; pwd"#, "/home/alice"),
        (
            r#"!{cd nope}"#,
            "error: Could not change directory to 'nope', not a directory",
        ),
        (r#"!{cd a b}"#, "error: Too many arguments to 'cd'"),
        (
            "!{nonexistent}",
            "error: Could not run 'nonexistent': No such file or directory",
        ),
        (r#"run "echo a  b" | .stdout"#, "a b\n"),
        (r#"run "echo" "a  b" | .stdout"#, "a  b\n"),
        (r#"run ["echo", "x"] | .stdout"#, "x\n"),
        (r#"run --standardIn="text" "cat" | .stdout"#, "text"),
        (r#"run --standardIn=[1, 2] "cat" | .stdout"#, "1\n2\n"),
        (r#"run --standardIn=[] "cat" | .stdout"#, ""),
        (r#""echo".run "a b" | .stdout"#, "a b\n"),
        ("(!{echo hi}).getClass", "os.ProcessResult"),
        ("!echo", "anonymous"),
    ]);
}

#[test]
fn uncaptured_output_goes_to_the_terminal() {
    let (result, platform) = with_big_stack(|| {
        let platform = Rc::new(platform());
        let mut ctx = Ctx::new(platform.clone());
        ctx.run("test", "!!{echo hi}").unwrap();
        ctx.run("test", "e = !echo").unwrap();
        ctx.run("test", r#"e "a" "b""#).unwrap();
        let result = ctx.run("test", r#"e ["c"]"#).unwrap();
        let output = platform.output.borrow().clone();
        (render(&result), output)
    });
    assert_eq!(result, "");
    assert_eq!(platform, "hi\na b\nc\n");
}

#[test]
fn mish_lines() {
    let output = with_big_stack(|| {
        let platform = Rc::new(platform());
        let mut ctx = Ctx::new(platform.clone());
        ctx.settings.mish = true;
        ctx.run("test", "cd sub").unwrap();
        ctx.run("test", "echo $(pwd)").ok();
        let result = ctx.run("test", "echo hi ${1 + 1}").unwrap();
        ctx.settings.mish = false;
        (render(&result), platform.output.borrow().clone())
    });
    assert_eq!(output.0, "");
    assert!(output.1.ends_with("hi 2\n"), "{:?}", output.1);
}

#[test]
fn the_env_global_is_the_child_environment() {
    let records = with_big_stack(|| {
        let (result, platform) = run_with(platform(), r#"env.FOO = "bar"; env.HOME = 42; !{true}.exitStatus"#);
        assert_eq!(result, "0");
        platform.processes.borrow().clone()
    });
    let env = &records[0].env;
    assert!(env.contains(&("FOO".to_string(), "bar".to_string())));
    assert!(env.contains(&("HOME".to_string(), "42".to_string())));
    assert!(env.contains(&("USER".to_string(), "alice".to_string())));
}

#[test]
fn a_command_killed_by_ctrl_c_interrupts_evaluation() {
    let result = with_big_stack(|| {
        let platform = platform().with_command("sleep", |_| FakeOutcome {
            exit_status: 130,
            interrupted: true,
            ..FakeOutcome::default()
        });
        run_with(platform, r#"try !{sleep 100} 42"#).0
    });
    assert_eq!(result, "interrupted");
}

#[test]
fn arguments_are_strings() {
    let records = with_big_stack(|| {
        let (_, platform) = run_with(platform(), r#"!{echo 1 true null "a/b"}; run "echo" [1, 2] 3"#);
        platform.processes.borrow().clone()
    });
    let argv: Vec<Vec<String>> = records.iter().map(|r| r.argv.clone()).collect();
    assert_eq!(
        argv,
        [vec!["echo", "1", "true", "null", "a/b"], vec!["echo", "[1, 2]", "3"]]
            .map(|v| v.into_iter().map(String::from).collect::<Vec<_>>())
    );
}
