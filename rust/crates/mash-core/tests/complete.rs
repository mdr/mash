//! Completion. Port of `completions/CompletionsTest.scala` (▶ marks the cursor), plus PathCompleterTest,
//! ContiguousRegionFinderTest, InvocationFinderTest and InvocationAssistanceTest.

use std::rc::Rc;

use mash_core::ast::{NodeIdGen, SourceUnit};
use mash_core::compile::{self, Settings};
use mash_core::complete::{
    self, CompletionResult, contiguous_region, find_invocation_with_literal_arg, path_completions,
};
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::platform::{MemoryFiles, TestPlatform};
use mash_syntax::{TextRange, TextSize};

#[derive(Debug)]
enum Case {
    Gives(&'static str, &'static [&'static str]),
    Contains(&'static str, &'static str),
    ContainsAll(&'static str, &'static [&'static str]),
    NotContains(&'static str, &'static str),
    /// Cases run with these files (paths ending in `/` are directories) and current directory
    Files(&'static [&'static str], &'static str, &'static [Case]),
    /// Cases run after evaluating some code
    Setup(&'static str, &'static [Case]),
}

impl Case {
    const fn gives(input: &'static str, expected: &'static [&'static str]) -> Case {
        Case::Gives(input, expected)
    }
    const fn contains(input: &'static str, expected: &'static str) -> Case {
        Case::Contains(input, expected)
    }
    const fn contains_all(input: &'static str, expected: &'static [&'static str]) -> Case {
        Case::ContainsAll(input, expected)
    }
    const fn not_contains(input: &'static str, unexpected: &'static str) -> Case {
        Case::NotContains(input, unexpected)
    }
    const fn files(files: &'static [&'static str], cwd: &'static str, cases: &'static [Case]) -> Case {
        Case::Files(files, cwd, cases)
    }
    const fn setup(setup: &'static str, cases: &'static [Case]) -> Case {
        Case::Setup(setup, cases)
    }
}

const CASES: &[Case] = &[
    Case::files(
        &[r#"/foo"#],
        r#"/"#,
        &[Case::contains(r#"▶"#, r#"ls"#), Case::contains(r#"▶"#, r#"foo"#)],
    ),
    Case::gives(r#"{ foo: 42 }.fo▶"#, &[r#"foo"#]),
    Case::gives(r#"{ bar: 1, baz: 2, buzz: 3 }.ba▶"#, &[r#"bar"#, r#"baz"#]),
    Case::contains(r#"{ foo: 1 } | _.▶"#, r#"foo"#),
    Case::contains(r#"[ { foo: 1 } ] | map (_.▶)"#, r#"foo"#),
    Case::gives(r#"[ { revert: 1 } ].rever▶"#, &[r#"reverse"#, r#"revert"#]),
    Case::gives(r#" "a string".startsW▶ "#, &[r#"startsWith"#]),
    Case::not_contains(r#" ''.histor▶ "#, r#"history"#),
    Case::files(
        &[r#"/home/alice/file.txt"#],
        r#"/"#,
        &[
            Case::gives(r#"cd /hom▶"#, &[r#"/home/"#]),
            Case::gives(r#"cd /home/al▶"#, &[r#"/home/alice/"#]),
            Case::gives(r#"cd "/home"/al▶"#, &[r#"/home/alice/"#]),
            Case::gives(r#"cd(/hom▶"#, &[r#"/home/"#]),
        ],
    ),
    Case::files(
        &[r#"/dir/"#, r#"/document.txt"#],
        r#"/"#,
        &[
            Case::gives(r#"cd ▶"#, &[r#"dir/"#]),
            Case::gives(r#"cd d▶"#, &[r#"dir/"#]),
            Case::gives(r#"cd pw▶"#, &[r#"pwd"#]),
            Case::gives(r#"cd(▶"#, &[r#"dir/"#]),
            Case::gives(r#"cd(d▶"#, &[r#"dir/"#]),
            Case::gives(r#"cd(pw▶"#, &[r#"pwd"#]),
            Case::gives(r#"d▶ | cd"#, &[r#"dir/"#]),
            Case::gives(r#"cd --directory="d"▶"#, &[r#"dir/"#]),
        ],
    ),
    Case::files(
        &[r#"/spaces in name"#],
        r#"/"#,
        &[Case::gives(r#"readLines spaces▶"#, &[r#"spaces in name"#])],
    ),
    Case::files(
        &[r#"/.dotfile"#],
        r#"/"#,
        &[Case::gives(r#"readLines .▶"#, &[r#"../"#, r#"./"#, r#".dotfile"#])],
    ),
    Case::files(
        &[r#"/readLines.txt"#],
        r#"/"#,
        &[Case::gives(r#"readLines.▶"#, &[r#"readLines.txt"#])],
    ),
    Case::gives(r#".▶"#, &[r#"../"#, r#"./"#]),
    Case::gives(r#"cd .▶"#, &[r#"../"#, r#"./"#]),
    Case::gives(r#"cd ..▶"#, &[r#"../"#]),
    Case::gives(r#"def foo (n = ls.permiss▶)"#, &[r#"permissions"#]),
    Case::gives(r#"(n = ls.permiss▶) => 42"#, &[r#"permissions"#]),
    Case::gives(r#"class Foo (n = ls.permiss▶)"#, &[r#"permissions"#]),
    Case::gives(r#"def foo (n = ls) = n.permiss▶"#, &[r#"permissions"#]),
    Case::gives(r#"(n = ls) => n.permiss▶"#, &[r#"permissions"#]),
    Case::gives(r#"class Foo (n = ls) { def foo = n.permiss▶ }"#, &[r#"permissions"#]),
    Case::files(
        &[r#"/readme.txt"#],
        r#"/"#,
        &[
            Case::gives(r#"read▶"#, &[r#"read"#, r#"readLines"#, r#"readme.txt"#]),
            Case::gives(r#"readme.t▶"#, &[r#"readme.txt"#]),
            Case::gives(r#" "readme.t▶" "#, &[r#"readme.txt"#]),
            Case::gives(r#" "readme".t▶ "#, &[r#"readme.txt"#]),
            Case::gives(r#"readme.▶"#, &[r#"readme.txt"#]),
        ],
    ),
    Case::files(
        &[r#"/readme.txt"#],
        r#"/"#,
        &[
            // (Scala ran these with `readme` as the only global)
            Case::gives(r#"readme.▶ # with binding"#, &[r#"readme.txt"#]),
            Case::gives(r#"readme.t▶ # with binding"#, &[r#"readme.txt"#]),
        ],
    ),
    Case::files(
        &[r#"/file.txt"#],
        r#"/"#,
        &[Case::gives(r#"readLines ▶"#, &[r#"file.txt"#])],
    ),
    Case::files(
        &[r#"/foobar.txt"#],
        r#"/"#,
        &[
            Case::gives(r#"[{ foobar: 42 }] | map fooba▶"#, &[r#"foobar"#]),
            Case::gives(r#"[{ bar: 42 }] | map fooba▶"#, &[r#"foobar.txt"#]),
        ],
    ),
    Case::gives(r#"{ foo: 42 } | select fo▶"#, &[r#"foo"#]),
    Case::gives(r#"[{ foo: 42 }] | select fo▶"#, &[r#"foo"#]),
    Case::gives(r#"pwd.children --recur▶"#, &[r#"--recursive"#]),
    Case::gives(r#"select --add▶"#, &[r#"--add"#]),
    Case::gives(r#"select --▶"#, &[r#"--add"#, r#"--selectors"#, r#"--target"#]),
    Case::gives(r#"cd -▶"#, &[r#"--directory"#]),
    Case::contains(r#"ls -▶"#, r#"-a"#),
    Case::contains(r#"'dir' | ls -▶"#, r#"-a"#),
    Case::contains(r#"def fun foobar = 42; fun --fooba▶"#, r#"--foobar"#),
    Case::contains(r#"def fun (@(shortFlag 'f') foobar) = 42; fun -▶"#, r#"-f"#),
    Case::contains(
        r#"class A { def meth foobar = 42 }; A.new.meth --fooba▶"#,
        r#"--foobar"#,
    ),
    Case::files(
        &[r#"/-"#],
        r#"/"#,
        &[Case::gives(r#"-▶"#, &[r#"-"#]), Case::contains(r#"ls -▶"#, r#"-"#)],
    ),
    Case::files(
        &[r#"/--"#],
        r#"/"#,
        &[Case::gives(r#"--▶"#, &[r#"--"#]), Case::contains(r#"ls --▶"#, r#"--"#)],
    ),
    Case::gives(r#"[{ foo: 42 }].map fo▶"#, &[r#"foo"#]),
    Case::gives(r#"map permiss▶ --sequence=ls"#, &[r#"permissions"#]),
    Case::gives(r#"{ foo: 42 }?.fo▶"#, &[r#"foo"#]),
    Case::contains(r#"{ foo: 42 }?.▶"#, r#"foo"#),
    Case::gives(r#"..▶"#, &[r#"../"#]),
    Case::gives(r#"[].maxBy.targe▶"#, &[r#"target"#]),
    Case::files(&[r#"/directory/"#], r#"/directory"#, &[]),
    Case::gives(r#"map --sequence=ls permiss▶"#, &[r#"permissions"#]),
    Case::files(
        &[r#"/dollar$ign"#],
        r#"/"#,
        &[
            Case::gives(r#" dollar▶ "#, &[r#"dollar`$ign"#]),
            Case::gives(r#" dollar`$i▶ "#, &[r#"dollar`$ign"#]),
            Case::gives(r#" dollar`$▶ "#, &[r#"dollar`$ign"#]),
            Case::gives(r#" dollar$▶ "#, &[r#"dollar`$ign"#]),
            Case::gives(r#" "dollar"▶ "#, &[r#"dollar`$ign"#]),
            Case::gives(r#" "dollar`$i"▶ "#, &[r#"dollar`$ign"#]),
            Case::gives(r#" "dollar`$"▶ "#, &[r#"dollar`$ign"#]),
            Case::gives(r#" "dollar$"▶ "#, &[r#"dollar`$ign"#]),
        ],
    ),
    Case::files(
        &[r#"/quotation"mark"#],
        r#"/"#,
        &[
            Case::gives(r#" quotation▶ "#, &[r#"quotation`"mark"#]),
            Case::gives(r#" "quotation▶"#, &[r#"quotation`"mark"#]),
        ],
    ),
    Case::files(
        &[r#"/home/alice/file.txt"#],
        r#"/"#,
        &[
            Case::gives(r#"~/file▶"#, &[r#"~/file.txt"#]),
            Case::gives(r#"~▶"#, &[r#"~/"#]),
        ],
    ),
    Case::gives(r#"pwd.info.toStrin▶"#, &[r#"toString"#]),
    Case::gives(r#"{}.toStrin▶"#, &[r#"toString"#]),
    Case::gives(r#"[4].toStrin▶"#, &[r#"toString"#]),
    Case::gives(r#"{}.▶toStrin"#, &[r#"toString"#]),
    Case::gives(r#"ls ▶--al"#, &[r#"--all"#]),
    Case::contains(r#"ls ▶-"#, r#"--all"#),
    Case::contains(r#"1▶."#, r#"days"#),
    Case::contains(r#"1▶?."#, r#"days"#),
    Case::files(
        &[r#"/file.txt"#],
        r#"/"#,
        &[
            Case::gives(r#" ("file."▶) "#, &[r#"file.txt"#]),
            Case::gives(r#" (▶"file.") "#, &[r#"file.txt"#]),
            Case::gives(r#"(/▶"#, &[r#"/file.txt"#]),
            Case::contains(r#"(▶"#, r#"file.txt"#),
            Case::gives(r#"readLines(file.▶"#, &[r#"file.txt"#]),
            Case::contains(r#"readLines(▶"#, r#"file.txt"#),
        ],
    ),
    // (Scala also checked that a list's own members aren't described as vectorised: see list_members_are_not_vectorised)
    Case::files(
        &[r#"/abc123xyz"#, r#"/foobar123baz"#],
        r#"/"#,
        &[
            Case::gives(r#"123▶"#, &[r#"abc123xyz"#, r#"foobar123baz"#]),
            Case::gives(r#"1▶"#, &[r#"abc123xyz"#, r#"foobar123baz"#]),
        ],
    ),
    Case::files(
        &[r#"/foo---"#, r#"/---foo"#],
        r#"/"#,
        &[Case::gives(r#"foo▶"#, &[r#"foo---"#])],
    ),
    Case::files(&[r#"/foo-bar"#], r#"/"#, &[Case::gives(r#"cd foo-▶"#, &[r#"foo-bar"#])]),
    Case::gives(r#"{ foobar: 42 }.foo▶ = 42"#, &[r#"foobar"#]),
    Case::gives(r#"a = { foobar: 42}.foo▶"#, &[r#"foobar"#]),
    Case::gives(r#"kill --signal=H▶"#, &[r#"HUP"#]),
    Case::files(
        &[r#"/.git"#],
        r#"/"#,
        &[
            Case::gives(r#"[{ gitty: 42 }].gi▶"#, &[r#"gitty"#]),
            Case::contains(r#"[{ gitty: 42 }].▶"#, r#"gitty"#),
            Case::gives(r#"[{ gitty: 42 }] .gi▶"#, &[r#".git"#]),
        ],
    ),
    Case::files(&[r#"/file"#], r#"/"#, &[Case::gives(r#"ls -r▶"#, &[])]),
    Case::gives(r#"{ foobar: 42 } | .foo▶"#, &[r#"foobar"#]),
    Case::files(
        &[r#"/.dotfiles/.bashrc"#],
        r#"/"#,
        &[
            Case::gives(
                r#"ls ".dotfiles/."▶"#,
                &[r#".dotfiles/../"#, r#".dotfiles/./"#, r#".dotfiles/.bashrc"#],
            ),
            Case::gives(
                r#"ls ".dotfiles/".▶"#,
                &[r#".dotfiles/../"#, r#".dotfiles/./"#, r#".dotfiles/.bashrc"#],
            ),
        ],
    ),
    // (Linux only in Scala: user names)
    Case::gives(r#" "${user.fullNam▶} "#, &[r#"fullName"#]),
    Case::gives(r#" "$user.fullNam▶ "#, &[r#"fullName"#]),
    Case::gives(r#" [{ foo: true }] | find fo▶"#, &[r#"foo"#]),
    Case::gives(r#" { ls.fir▶ : 42 }"#, &[r#"first"#]),
    Case::gives(
        r#"(foo bar => foo.perm▶) --bar=100 --foo=ls.first"#,
        &[r#"permissions"#],
    ),
    Case::gives(r#"def foo = { ls.perm▶"#, &[r#"permissions"#]),
    Case::gives(r#"foobar => foob▶"#, &[r#"foobar"#]),
    Case::gives(r#"{ foobar } => foob▶)"#, &[r#"foobar"#]),
    Case::gives(r#"foobar = 42; foob▶"#, &[r#"foobar"#]),
    Case::gives(r#"{ foobar } = { foobar: 42 }; foob▶"#, &[r#"foobar"#]),
    Case::gives(
        r#"{ foobar: { bazzle } } = { foobar: { bazzle: 42 } }; bazz▶"#,
        &[r#"bazzle"#],
    ),
    Case::gives(
        r#"{ foobar: { bazzle: buzzle } } = { foobar: { bazzle: 42 } }; buzz▶"#,
        &[r#"buzzle"#],
    ),
    Case::gives(r#"def fun foobar = foob▶"#, &[r#"foobar"#]),
    Case::gives(r#"foobundle = 42; { foobar = 42 }; foob▶"#, &[r#"foobundle"#]),
    Case::gives(r#"[a, b, foobar] = [1, 2, 3]; foob▶"#, &[r#"foobar"#]),
    Case::gives(r#"[a, b, foobar] = [1, 2]; foob▶"#, &[r#"foobar"#]),
    Case::gives(r#"class Bob { def bob = [].reve▶"#, &[r#"reverse"#]),
    Case::gives(r#"class Point x y; Poin▶"#, &[r#"Point"#]),
    Case::gives(
        r#"{ class Rectangle width height { def area = width * height }; Rectangle(3, 4).are▶ }"#,
        &[r#"area"#],
    ),
    Case::gives(
        r#"{ class Rectangle width height; Rectangle(3, 4).wid▶ }"#,
        &[r#"width"#],
    ),
    Case::contains(r#"class Foo { def foobar = 42 }; Foo.new.▶"#, r#"foobar"#),
    Case::contains(r#"class Foo { def foobar n = n.▶ }"#, r#"toString"#),
    Case::gives(r#"class Foo wibble { def foo = wibb▶ }"#, &[r#"wibble"#]),
    Case::gives(r#"class Foo { def foo wibble = wibb▶ }"#, &[r#"wibble"#]),
    Case::contains(
        r#"class A { def method = 42; def anotherMethod = metho▶ }"#,
        r#"method"#,
    ),
    Case::contains(
        r#"class A { def method = 42; def anotherMethod = this.metho▶ }"#,
        r#"method"#,
    ),
    Case::not_contains(r#"class A { def method = fiel▶ }"#, r#"fields"#),
    Case::contains(r#"class A { def method = this.fiel▶ }"#, r#"fields"#),
    Case::setup(
        r#"class A { @private def privateMethod = 42 }"#,
        &[Case::not_contains(r#"A.new.privateMetho▶"#, r#"privateMethod"#)],
    ),
    Case::files(
        &[r#"/.dotfile"#],
        r#"/"#,
        &[Case::contains(r#"42 | .▶"#, r#"toString"#)],
    ),
    Case::contains_all(r#"Object.▶"#, &[r#"merge"#, r#"parent"#]),
    Case::contains(r#"Object.merg▶"#, r#"merge"#),
    Case::contains(r#"Object.paren▶"#, r#"parent"#),
    Case::contains_all(r#"class Rect w h; Rect.▶"#, &[r#"new"#, r#"parent"#]),
    Case::contains(r#"class Rect w h; Rect.ne▶"#, r#"new"#),
    Case::contains(r#"class Rect w h; Rect.paren▶"#, r#"parent"#),
    Case::setup(
        r#"fieldAndValue = { foo: 42 }.fields.first; fieldAndValue.extraField = 100"#,
        &[Case::contains(r#"fieldAndValue.extraFiel▶"#, r#"extraField"#)],
    ),
    Case::setup(
        r#"class Point x y; p = Point 3 4; p.extraField = 100"#,
        &[Case::contains(r#"p.extraFiel▶"#, r#"extraField"#)],
    ),
    Case::contains(r#"def foo bar = (ba▶"#, r#"bar"#),
    Case::contains(r#"def foo bar = (▶"#, r#"bar"#),
    Case::contains(r#"class Thing field; Thing --fiel▶"#, r#"--field"#),
    Case::contains(r#"import core.help.▶"#, r#"help"#),
    Case::contains(r#"import core.help.h▶"#, r#"help"#),
    Case::contains(r#"import c▶"#, r#"core"#),
    Case::contains(r#"'1234'.all (.isD▶"#, r#"isDigit"#),
    Case::contains(r#"'1234'[0].isD▶"#, r#"isDigit"#),
    Case::contains(r#"ls | groupBy (.perm▶"#, r#"permissions"#),
    Case::contains(r#"ls | countBy (.perm▶"#, r#"permissions"#),
    Case::contains(r#"ls | countBy --groups (.extension) | sortBy (.co▶"#, r#"count"#),
    Case::contains(
        r#"!{git log --oneline} | parse '{hash} {subject}' | map (.sub▶"#,
        r#"subject"#,
    ),
    // New: the file functions of the data namespaces complete files; members of http responses and hosts
    Case::files(
        &[r#"/file.txt"#],
        r#"/"#,
        &[
            Case::gives(r#"json.read ▶"#, &[r#"file.txt"#]),
            Case::gives(r#"json.write ▶"#, &[r#"file.txt"#]),
            Case::gives(r#"xml.read ▶"#, &[r#"file.txt"#]),
            Case::gives(r#"yaml.read ▶"#, &[r#"file.txt"#]),
            Case::gives(r#"toml.read ▶"#, &[r#"file.txt"#]),
            Case::gives(r#"yaml.write ▶"#, &[r#"file.txt"#]),
            Case::gives(r#"csv.write ▶"#, &[r#"file.txt"#]),
            Case::gives(r#"mash.source ▶"#, &[r#"file.txt"#]),
        ],
    ),
    Case::gives(r#"http.get 'http://example.com' | .succ▶"#, &[r#"succeeded"#]),
    Case::contains(r#"(http.get 'http://example.com').headers.▶"#, r#"name"#),
    Case::contains(r#"(http.get 'http://example.com').cookies.va▶"#, r#"value"#),
    Case::gives(r#"(net.url 'http://example.com').host.ipAddr▶"#, &[r#"ipAddresses"#]),
    Case::contains(r#"http.get --ins▶"#, r#"--insecure"#),
];

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];

#[derive(Clone, Default)]
struct Context {
    files: &'static [&'static str],
    cwd: &'static str,
    setup: Option<&'static str>,
}

/// Completions (their insert text) of `input`, where ▶ marks the cursor
fn completions(context: &Context, input: &str) -> Vec<String> {
    full_completions(context, input)
        .map(|r| r.completions.iter().map(|c| c.insert_text().to_string()).collect())
        .unwrap_or_default()
}

fn full_completions(context: &Context, input: &str) -> Option<CompletionResult> {
    let pos = input.find('▶').expect("cursor marker");
    let text = input.replacen('▶', "", 1);
    let platform = TestPlatform {
        files: MemoryFiles::of(context.files),
        cwd: context.cwd.to_string(),
        ..TestPlatform::default()
    };
    let mut ctx = Ctx::new(Rc::new(platform));
    if let Some(setup) = context.setup {
        ctx.run("setup", setup).expect("setup runs");
    }
    complete::complete(&mut ctx, &text, pos, false)
}

fn check(context: &Context, case: &Case, failures: &mut Vec<String>, passing_deferred: &mut Vec<String>) {
    let (input, problem) = match case {
        Case::Files(files, cwd, cases) => {
            let context = Context {
                files,
                cwd,
                ..context.clone()
            };
            return cases
                .iter()
                .for_each(|c| check(&context, c, failures, passing_deferred));
        }
        Case::Setup(setup, cases) => {
            let context = Context {
                setup: Some(setup),
                ..context.clone()
            };
            return cases
                .iter()
                .for_each(|c| check(&context, c, failures, passing_deferred));
        }
        Case::Gives(input, expected) => {
            let actual = completions(context, input);
            (
                input,
                (actual != *expected).then(|| format!("got {actual:?}, expected {expected:?}")),
            )
        }
        Case::Contains(input, expected) => {
            let actual = completions(context, input);
            (
                input,
                (!actual.iter().any(|c| c == expected)).then(|| format!("{actual:?} doesn't contain {expected:?}")),
            )
        }
        Case::ContainsAll(input, expected) => {
            let actual = completions(context, input);
            let missing: Vec<_> = expected.iter().filter(|e| !actual.iter().any(|c| c == *e)).collect();
            (
                input,
                (!missing.is_empty()).then(|| format!("{actual:?} doesn't contain {missing:?}")),
            )
        }
        Case::NotContains(input, unexpected) => {
            let actual = completions(context, input);
            (
                input,
                actual
                    .iter()
                    .any(|c| c == unexpected)
                    .then(|| format!("{actual:?} contains {unexpected:?}")),
            )
        }
    };
    // Deferred cases are identified by input, plus the completion for "contains" cases (the same input can
    // appear with different expectations)
    let key = match case {
        Case::Contains(input, x) | Case::NotContains(input, x) => format!("{input} ∋ {x}"),
        _ => input.to_string(),
    };
    match (problem, DEFERRED.contains(&key.as_str())) {
        (Some(problem), false) => failures.push(format!("{key:?}: {problem}")),
        (None, true) => passing_deferred.push(format!("{key:?}")),
        _ => {}
    }
}

#[test]
fn completions_test() {
    let (failures, passing_deferred) = with_big_stack(|| {
        let mut failures = vec![];
        let mut passing_deferred = vec![];
        let context = Context {
            cwd: "/",
            ..Context::default()
        };
        for case in CASES {
            check(&context, case, &mut failures, &mut passing_deferred);
        }
        (failures, passing_deferred)
    });
    let mut message = String::new();
    if !failures.is_empty() {
        message += &format!("{} failing case(s):\n  {}\n", failures.len(), failures.join("\n  "));
    }
    if !passing_deferred.is_empty() {
        message += &format!(
            "{} deferred case(s) now pass:\n  {}\n",
            passing_deferred.len(),
            passing_deferred.join("\n  ")
        );
    }
    assert!(message.is_empty(), "{message}");
}

#[test]
fn list_members_are_not_vectorised() {
    with_big_stack(|| {
        let result = full_completions(
            &Context {
                cwd: "/",
                ..Context::default()
            },
            "[1].isEmpt▶",
        )
        .unwrap();
        let completion = result.completions.iter().find(|c| c.display == "isEmpty").unwrap();
        assert!(!completion.description.as_deref().unwrap().contains("vectorised"));
    })
}

// ------------------------------------------------------------------------------------------------ PathCompleterTest

fn path_completions_with(files: &[&str], search: &str, substring: bool) -> Vec<String> {
    let platform = TestPlatform {
        files: MemoryFiles::of(files),
        cwd: "/".into(),
        ..TestPlatform::default()
    };
    path_completions(&platform, search, false, substring)
        .into_iter()
        .map(|c| c.path)
        .collect()
}

#[test]
fn path_completion() {
    assert_eq!(path_completions_with(&["/file.txt"], "file", false), ["file.txt"]);
    assert_eq!(path_completions_with(&["/dir/"], "di", false), ["dir/"]);
    assert_eq!(
        path_completions_with(&["/foo1/", "/foo2", "/foo3", "/bar"], "foo", false),
        ["foo1/", "foo2", "foo3"]
    );
    assert_eq!(path_completions_with(&["/root"], "/roo", false), ["/root"]);
    assert_eq!(path_completions_with(&["/dir/file"], "dir/f", false), ["dir/file"]);
    assert_eq!(path_completions_with(&["/dir/file"], "/dir/f", false), ["/dir/file"]);
    assert_eq!(
        path_completions_with(&["/dir/", "/file", "/.dotfile"], "", false),
        ["dir/", "file"]
    );
    assert_eq!(path_completions_with(&["/file", "/.dotfile"], "/", false), ["/file"]);
    assert_eq!(path_completions_with(&["/.dotfile"], ".dot", false), [".dotfile"]);
    assert_eq!(path_completions_with(&[], "..", false), ["../"]);
    assert_eq!(path_completions_with(&[], ".", false), ["./", "../"]);
    assert_eq!(path_completions_with(&[], "/.", false), ["/./", "/../"]);
    assert_eq!(
        path_completions_with(&["/___123__", "/__123___"], "123", true),
        ["__123___", "___123__"]
    );
}

// ------------------------------------------------------------------------------------------------ ContiguousRegionFinderTest

fn region(pattern: &str) -> TextRange {
    let start = pattern.find('#').unwrap();
    TextRange::at(
        TextSize::from(start as u32),
        TextSize::from(pattern.matches('#').count() as u32),
    )
}

#[track_caller]
fn check_region(s: &str, initial: &str, expected: &str, liberal: bool) {
    assert_eq!(
        contiguous_region(s, region(initial), false, liberal),
        region(expected),
        "in {s:?}"
    );
}

#[test]
fn contiguous_regions() {
    check_region("read", "  # ", "####", true);
    check_region("the quick brown", "      ##       ", "    #####      ", true);
    check_region(
        "the 'quick brown' fox",
        "          #          ",
        "    #############    ",
        true,
    );
    check_region("/usr/local/bin", "             #", "##############", true);
    check_region("foo bar baz", "    #      ", "    ###    ", true);
    check_region("(ls)", " #  ", " ## ", false);
    check_region("(/App)", " #    ", " #### ", false);
}

// ------------------------------------------------------------------------------------------------ InvocationFinderTest

fn invocation_pos(s: &str) -> usize {
    let compiled = compile::compile_forgiving(
        SourceUnit::new("test", s),
        Settings::default(),
        &Default::default(),
        &mut NodeIdGen::default(),
    );
    let literal = mash_syntax::lexer::tokenise(s, false)
        .tokens
        .into_iter()
        .find(|t| t.kind == mash_syntax::SyntaxKind::STRING_LITERAL)
        .unwrap();
    find_invocation_with_literal_arg(&compiled.program.body, literal.range)
        .unwrap()
        .arg_pos
}

#[test]
fn invocation_finder() {
    assert_eq!(invocation_pos("f 'arg'"), 0);
    assert_eq!(invocation_pos("f arg1 'arg2' arg3"), 1);
    assert_eq!(invocation_pos("f --arg1 'arg2' arg3"), 1);
    assert_eq!(invocation_pos("f('arg')"), 0);
    assert_eq!(invocation_pos("f(arg1, 'arg2', arg3)"), 1);
    assert_eq!(invocation_pos("f --arg='arg'"), 0);
    assert_eq!(invocation_pos("f arg1 --arg2='arg2'"), 1);
    assert_eq!(invocation_pos("'arg' | f"), 0);
    assert_eq!(invocation_pos("'arg2' | f arg1"), 1);
    assert_eq!(invocation_pos("arg2 | f --arg1='arg1'"), 0);
}

// ------------------------------------------------------------------------------------------------ InvocationAssistanceTest

fn nearest_function(input: &str) -> String {
    with_big_stack({
        let input = input.to_string();
        move || {
            let pos = input.find('▶').unwrap();
            let text = input.replacen('▶', "", 1);
            let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
            match complete::type_of_nearest_function(&mut ctx, &text, pos, false) {
                Some(ty) => format!("{ty:?}"),
                None => "<none>".into(),
            }
        }
    })
}

#[test]
fn invocation_assistance() {
    // Scala's cases use ls and home (M5); these use core and collections functions in the same positions
    assert_eq!(nearest_function("reverse▶"), "BuiltinFunction(collections.reverse)");
    assert_eq!(nearest_function("rev▶erse"), "BuiltinFunction(collections.reverse)");
    assert_eq!(nearest_function("▶reverse"), "BuiltinFunction(collections.reverse)");
    assert_eq!(nearest_function("reverse ▶"), "BuiltinFunction(collections.reverse)");
    assert_eq!(
        nearest_function("reverse 'dir'▶"),
        "BuiltinFunction(collections.reverse)"
    );
    assert_eq!(
        nearest_function("sort --descending▶"),
        "BuiltinFunction(collections.sort)"
    );
    assert_eq!(nearest_function("reverse now▶"), "BuiltinFunction(time.now)");
    assert_eq!(nearest_function("reverse ▶now"), "BuiltinFunction(time.now)");
    assert_eq!(nearest_function("reverse▶ now"), "BuiltinFunction(collections.reverse)");
    assert_eq!(nearest_function("reverse (▶now)"), "BuiltinFunction(time.now)");
    assert_eq!(
        nearest_function("reverse ▶(now)"),
        "BuiltinFunction(collections.reverse)"
    );
    assert_eq!(
        nearest_function("1.toString▶"),
        "BoundBuiltinMethod(core.Number, toString)"
    );
    assert_eq!(
        nearest_function("''.startsWith 'x'▶"),
        "BoundBuiltinMethod(core.String, startsWith)"
    );
}
