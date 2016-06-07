---
layout: default
title: Quick Reference Guide
permalink: /quick-reference-guide/
---

# Mash Quick Reference Guide

## Language

### Literals

* Booleans: `true`, `false`
* Numbers: `42`, `0.5`
* Null: `null`
* Sequences: `[1, 2, 3]`
* Objects: `{ foo: 42, bar: 42 }`
* Strings: `'basic string'`, `"rich string"`

Double-quoted rich strings are recommended for most interactive uses, and provide the following features:

* Simple interpolation: `"My name is $user.name"`
* Full expression interpolation: `"My name is ${user.name | reverse}"`
* An initial tilde (~) is replaced with the current user's home directory
* Tagged as `os.Path`, which provides methods for path manipulation: `"foo.txt".lastModified`

If the *bare words* are enabled (configuration property `language.bareWords`), then any
identifier that is not bound is automatically promoted to a rich string.

### Conditionals, truthiness, and/or
* Conditional expression: `if` .. `then` .. `else` ..

Mash considers the following values *falsey*: `false`, `0`,`[]`, `{}`, `""`. All other
values are *truthy*.

The `and` and `or` operators work as follows:

* The `and` operator first evaluates its left argument. If it is falsey, it returns the left
  argument, and leaves the right unevaluated. Otherwise, it evaluates and returns the right
  argument.
* Similarly, the `or` operator first evaluates its left argument. If it is truthy, it
  returns the left argument, and leaves the right argument unevaluated. Otherwise, it
  evaluates and returns the right argument.

For example:

    true and true # true
    dir.exists or dir.mkdir
	dir.exists and dir.cd

`not` is a standard library function.

### Pipe operator

The pipe operator `|` is syntax sugar for function application:

* `a | f` is equivalent to `f a`
* `a | f b` is equivalent to `f b a`

### Lambdas and holes

Anonymous functions:

    x => x + 1
    x => x.size

As syntax sugar, anonymous functions can be written using the *hole* (`_`) operator. Same
examples as above:

    _ + 1
    _.size

The scope of the hole is:

* Nearest enclosing parentheses: `(_ + 1)`
* A non-leftmost pipe segment: `42 | _ + 1`

Holes can be repeated:

    _ * _ # equivalent to x => x * x

There is a further syntax sugar for member expressions:

    .size # equivalent to _.size, or x => x.size

### Function calls:

Ordinary function call syntax:

    f arg1 arg2 arg3

Named argument syntax:

    f --param1=arg1 --param2=arg2 --param3=arg3

Alternative parenthesis-based function call syntax:

    f(arg1, arg2, arg3)

### Member vectorisation

If a member does not exist on a sequence object, but it does on the elements of the
sequence, then a member access is automatically vectorised:

    ls.permissions # returns a list of Permissions objects

### Mish

Mish is a sublanguage within Mash, for launching processes.

* `!ps` - run a command
* `!{which java}` - run a command with arguments; the process output is captured
* `!!{nano file}` - run a command, and connect stdin/stdout to terminal; no output is captured
* `!{gnome-open $file}` - can embed Mash variables Mish
* `!{gnome-open ${file}}` - or entire Mash fragments inside a `${}` region
* `ls | first | !nano` - run a process as a function

By default, Mash is in "Mash" mode. If a command line is prefixed with `!`, the entire line
is interpreted as a Mish command. If `!` is issued by itself as a command, then Mash toggles
into Mish by default.

### Other features
* Arithmetic operators: `+`, `-`, `*`, `/`
* Comparisons: `a < b`, `a <= b`, `a > b`, `a >= b`
* Chained comparisions: `0 <= n < 10`
* Assignment to global variables: `a = 42`
* Define a global function: `def square n = n * n`
* Lookup expressions: `list[2]` (third element), `list[-1]` (last element),
  `obj["toString"]` (toString member)
* Null-safe dereference: `file?.extension?.toUpper`
* Help operator: `function?`, `obj.method?`
* Strings can be called as functions which lookup an object member with that name:
  `"permissions" pwd`

## Command line

### Hotkeys:
* Tab: Attempt completion
* Ctrl-space: show syntax for current function/method
* Ctrl-r: incremental history search
* Ctrl-q: quote current word
* Ctrl-p: previous history
* Ctrl-n: next history
* Alt-.: insert last argument
* Alt-f: forward word
* Alt-b: backward word
* Ctrl-e: end of line
* Ctrl-a: beginning of line
* Ctrl-l: clear screen
* Ctrl-k: kill line
* Alt-,: toggle mish prefix for this line

### Tab completion

Hit tab once to complete methods, functions, files, arguments, etc. The list of possible
completions is displayed. If you hit tab again immediately, you enter completion browsing
mode, which will display additional information about the options.

### mashrc

Mash executes commands from `~/.mash/mashrc` on startup, and can be used to add aliases and
set configuration options.

### Configuration options

   config.language.bareWords = true # Treat unbound identifiers as string literals
   config.cli.showStartupTips = true # Show tip on startup

## Standard library

### Directory navigation

* `cd "dir"`
* `cd` (home directory)
* `up` (go up one directory)
* `up 2` (go up 2 directories)
* `pwd`
* `home`
* `oldDirs` (list of previous directories)
* `oldDirs.last.cd` (change to most recent previous directory)

### Working with files

* `file.permissions.owner.canExecute`
* `readLines file` / `cat file`
* `delete file` / `rm file`
* `file.moveInto "dir/"`
* `file.copyInto "dir/"`
* `file.rename "newName"`

### Collection processing

* map: `["foo", "bar", "baz"] | map reverse` ⇒ `["oof", "rab", "zab"]`
* where: `ls | where "isDirectory"`
* first: `ls | first`, `ls | first 10`
* last: `ls | last`, `ls | last 10`
* groupBy: `ls -r | groupBy "extension"`

### Processes, users and groups

* `user`, `users`, `groups`
* `groups | first | _.users`
* `ps`

### Dates and times

* `now`
* `2.days.ago`
* `3.hours.fromNow`
