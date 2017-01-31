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

### String literals
* Basic strings: `'basic string'`
* Rich strings: `"rich string"`

Rich strings are recommended for most interactive uses, and provide the following additional features
compared to basic strings:

* Variable interpolation: `"The current directory is $pwd, and the parent directory is $pwd.parent"`
* Full expression interpolation: `"The current directory backwards is: ${pwd | reverse}"`
* An initial tilde character (`~`) is expanded to the current user's home directory
* Tagged as `os.Path`, which provides methods for path manipulation: e.g. `"foo.txt".lastModified`

Both single- and double-quoted strings can span multiple lines; any newline characters are retained
literally in the string value.

String escapes sequences start with the backtick character `` ` ``:

* `` `n `` - newline
* `` `r `` - carriage return
* `` `t `` - tab
* `` `$ `` - `$`
* `` `' `` - `'`
* `` `" `` - `"`

If *bare words* are enabled (using configuration property `language.bareWords`), then any
unbound identifier is automatically promoted to a string tagged with `os.Path`.

### Object literals

* Basic object literal: `{ foo: 42, bar: 42 }`.

An arbitrary expression can appear as a field name, but an identifier must be enclosed in parens for this to work:

    fieldName = "foo"
    { (fieldName): 42 } # => { foo: 42 }

If a variable is in scope, it can be used to name a field and provide the value in one shot:

    value = "foo"
    { value } # => { value: "foo" }

### Conditionals, truthiness, and/or
* Conditional expression: `if` .. `then` .. `else` ..

Mash considers the following values *falsey*: `false`, `null`, `0`,`[]`, `{}`, `""`. All other
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

### Function declarations

Functions are declared using `def` syntax:

    def square n = n * n

Multiple parameters are supported:

    def add x y = x + y

Zero parameters:

    def greet = print "Hello"

Variadic parameters:

    def printAll words... = words | each print

Default arguments:

    def someFunc (n = 42) = ....

Lazy arguments:

    def someFunc (@lazy block) = ...

Functions can be nested:

    def outer = {
      def inner n = n + 1
      inner 42
    }

### Function calls:

Ordinary function call syntax:

    f arg1 arg2 arg3

Named argument syntax:

    f --param1=arg1 --param2=arg2 --param3=arg3

Alternative parenthesis-based function call syntax:

    f(arg1, arg2, arg3)

### Nullary functions

Some functions can be called with no arguments. For example, `ls` or `now`. If such a nullary function is referenced either using an identifier, or by a member expression (e.g. `git.status`), then it is immediately invoked.

If you need to obtain a reference to a nullary function or method itself, you can use a lookup expression.

    git["status"]
    time["now"]

### Anonymous functions and holes

Anonymous functions with a single argument:

    x => x + 1
    x => x.size

As syntax sugar, anonymous functions with a single argument can be written using the *hole* (`_`) operator. Same
examples as above:

    _ + 1
    _.size

The scope of the hole is the smallest enclosing:

* Pair of parentheses: `(_ + 1)`
* Block: `{ _ + 1 }`
* Non-leftmost pipe segment: `42 | _ + 1`
* Function or lambda body: `def adder n = _ + n`
* Program

Holes can be repeated:

    _ * _ # equivalent to x => x * x

There is a further abbreviation for member expressions:

    .size # equivalent to _.size, or x => x.size

Multiple parameter functions are supported:

    x y => x * y

As are empty parameter lists.

    => now + 1.hour

### Member vectorisation

If a member does not exist on a sequence object, but it does on the elements of the
sequence, then a member access is automatically vectorised:

    ls.permissions # returns a list of Permissions objects

### Mish and subprocesses

Mish is a sublanguage within Mash for launching processes, with a similar syntax to
 a traditional shell.

By default, Mash is in "regular Mash" mode. If a command line starts or ends with `!`, the entire line
 is interpreted as a Mish command. If `!` is issued by itself as a command, then Mash toggles
 into Mish by default, and the prompt changes from a `$` to a `!`.

Within Mash, you can embed Mish fragments. A `!{..}` expression runs a subprocess, captures the output, and returns a `ProcessResult` object. For example:
* !{which java}.toPath
* !{locate python}.lines
* !{sleep 3}.duration

However, standard input is not connected to the subprocess when using `!{..}`. If you want to interact with a process interactively, then you can use a `!!{..}` expression:
* !!{nano file}

Conversely, you can embed Mash fragments within Mish:

* `gnome-open $file`
* `gnome-open ${file}`

### Other features
* Arithmetic operators: `+`, `-`, `*`, `/`
* Comparisons: `a < b`, `a <= b`, `a > b`, `a >= b`
* Chained comparisions: `0 <= n < 10`
* Assignment: `a = 42`, `foo.bar = 100`, `a += 1`
* Lookup expressions: `list[2]` (third element), `list[-1]` (last element),
  `obj["toString"]` (toString member)
* Null-safe dereference: `file?.extension?.toUpper`
* Help operator: `function?`, `obj.method?`
* Strings can be invoked as functions. The effect is to lookup a member with that name:
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

### Startup file: `init.mash`

Mash executes commands from `~/.mash/init.mash` on startup and can be used to add aliases and
set configuration options.

### Configuration options

    config.language.bareWords = false # Treat unbound identifiers as string literals
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
* `within "~/project" git.pull` (temporarily change directory, run `git.pull`, then restore the working directory)

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

* `ps`
* `user`, `users`, `groups` # Linux only
* `groups | first | _.users` # Linux only

### Dates and times

* `now`
* `2.days.ago`
* `3.hours.fromNow`
* `now + 3.weeks`

### Git support

* `git`: git namespace object, lists available functions
* `git.log`: return list of commit objects for the current repository
* `git.stage`: stage arguments for commit
* `git.unstage`: unstage arguments for commit
* `git.branches`: list local branches
