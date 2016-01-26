---
layout: default
title: Getting Started Guide
permalink: /getting-started-guide/
---

# Mash Getting Started Guide

This is an introductory tutorial to using Mash, an object shell for Linux.

## Working with objects

Our prompt shows us the current working directory. In this case, we are in the root of the filesystem:

    / $

Listing files can be done with `ls`:

    / $ ls
	╔═════╤════╤════╤═════╤═════╤═══════════╤═════════════╗
	║path │type│size│owner│group│permissions│lastModified ║
	╟─────┼────┼────┼─────┼─────┼───────────┼─────────────╢
	║app  │dir │4KB │root │root │rwxr-xr-x  │8 minutes ago║
	║bin  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║boot │dir │4KB │root │root │rwxr-xr-x  │4 months ago ║
	║dev  │dir │380B│root │root │rwxr-xr-x  │4 minutes ago║
	║etc  │dir │4KB │root │root │rwxr-xr-x  │4 minutes ago║
	║home │dir │4KB │root │root │rwxr-xr-x  │4 months ago ║
	║lib  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║lib64│dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║media│dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║mnt  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║opt  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║proc │dir │0B  │root │root │r-xr-xr-x  │4 minutes ago║
	║root │dir │4KB │root │root │rwx------  │4 minutes ago║
	║run  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║sbin │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║srv  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║sys  │dir │0B  │root │root │r-xr-xr-x  │4 minutes ago║
	║tmp  │dir │4KB │root │root │rwxrwxrwx  │4 minutes ago║
	║usr  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	║var  │dir │4KB │root │root │rwxr-xr-x  │1 month ago  ║
	╚═════╧════╧════╧═════╧═════╧═══════════╧═════════════╝

In a traditional Unix shell, the output of `ls` would be several lines of plain text. Mash
is, by contrast, an *object shell*, and the output is given as a sequence of objects, each
of which contains summary information about a file or directory, such as its name, size,
owner, and so on.

Mash will, by default, render a sequence of objects as a table, with a column for each field
in the object, and a row for each object in the sequence.

Notice that some fields are rendered in a particular human-readable way, for example, `4KB`,
`rwxr-xr-x`, or `1 month ago`. These are custom display formats for those fields, not text;
in the underlying objects, these values are stored as numbers, dates, or other objects.

We can chain the output of a function into another function using the pipe operator `|`:

	/ $ ls | first
	╔════════════╤══════════════╗
	║path        │app           ║
	║type        │dir           ║
	║size        │4KB           ║
	║owner       │root          ║
	║group       │root          ║
	║permissions │rwxr-xr-x     ║
	║lastModified│15 minutes ago║
	╚════════════╧══════════════╝

The `first` function selects the first item from a sequence; in this case, the object
summarising the `app` directory. Mash renders a single object as a table with a row for each
field.

An alternative object-oriented style is to use member syntax, calling the `first` method, to
produce the same result:

	/ $ ls.first
	...
	
We can drill successively deeper into the file object to request further information
(pressing the Up arrow key will recall commands from history).

	/ $ ls.first.permissions
	╔══════╤═══╗
	║owner │rwx║
	║group │r-x║
	║others│r-x║
	╚══════╧═══╝
	
	/ $ ls.first.permissions.others
	╔══════════╤═════╗
	║canRead   │true ║
	║canWrite  │false║
	║canExecute│true ║
	╚══════════╧═════╝

	/ $ ls.first.permissions.others.canWrite
    false

### The `it` variable

The variable `it` is bound to the result of the previously executed command. So the previous
transcript could have been instead:

	/ $ ls
    / $ it.first
    / $ it.permissions
    / $ it.others
    / $ it.canWrite

## Tab completion

Mash supports tab completion to provide contextual help. For example, we can explore the
available methods and fields on an object. If we press the Tab key at the position marked
with the arrow (`▶`):

    / $ ls.first.perm▶

Mash will expand it completely, as there is only one unique completion.

	/ $ ls.first.permissions

If there are multiple options, Mash will insert the common prefix, and then display the
available options, but remain in regular editing mode:

	/ $ ls.first.pa▶
	parent  path

If we press Tab a second time, Mash will go into *completions browser* mode, letting us
navigate the options with `Tab`, `Shift`+`Tab`, and the arrow keys. Pressing `Enter` will
accept the currently selected option.

	/ $ ls.first.parent
	parent  path
	┌─ Method ────────────────────────────────────────┐
	│ The parent of this path (method in PathSummary) │
	└─────────────────────────────────────────────────┘

### Path completion and quoting

Paths in Mash are represented as strings:

	/ $ ls "tmp"

Tab completion for paths will automatically add in quotes, for example:

	/ $ ls tm▶
    / $ ls "tmp/"

You also can use the shortcut Ctrl-q to quote the word at the current cursor position.

Mash supports *bare words*: if enabled, any identifier not currently bound to a variable is
automatically promoted to a string:

    / $ ls tmp

Bare words are disabled by default, but can be enabled by setting the appropriate
configuration option:

    / $ config.language.bareWords = true

This can be made permanent by adding the line to `~/.mash/mashrc`.

## Object browser

If `ls` returns a list of objects larger than can fit on a single page, Mash will display
them within a pager that lets us browse through the objects interactively. The supported
keys are:

* `q`: quit browser
* Up/down arrow keys: move up or down one item
* Page Up/Down: move up or down by a page
* Home / `g`: go to top of list
* End / `G`: go to bottom of list

## Collection processing

Mash contains a number of functions and methods designed to help manipulate and query
collections of objects. We'll look at a couple of examples in this section:

### Task 1: Finding the largest `.so` file

Suppose we want to find the largest shared object file (`.so`) within `/lib`.

    / $ cd "lib"

Let's start off with listing all the files in the directory, recursively:

	/ $ ls --recursive "/lib"

`--recursive` is a *flag* which modifies the behaviour of `ls` to recursively retrieve
results from subdirectories. For brevity, flags can often be provided in an alternative
single character form, in this case, `-r`:

	/ $ ls -r "/lib"

We want to return just the files that end in `.so`. We can use the `where` function to
filter the results:

	/ $ ls -r "/lib" | where (f => f.extension == "so")

`where` is a function that takes as argument a predicate (a function which produces a
boolean value), and applies it to a sequence, returning all the items in the sequence for
which the predicate holds true.

The construct `(f => f.extension == "so")` is a *closure* &mdash; an anonymous function
&mdash; that takes a file object as input, and tests whether or not it has the `so`
extension.

Mash supports a more concise syntax for closures using the underscore character (`_`), or
*hole*, which is syntax sugar for the above:

	/ $ ls -r "/lib" | where (_.extension == "so")

Now we want to find the largest file from amongst our results. We can use the `maxBy`
function for this:

	/ $ ls -r "/lib" | where (_.extension == "so") | maxBy (_.size)

If we are projecting a member (that is, a field or method) of an object, as we do with
`(_.size)`, a shorthand is to use the member name as a string:

	/ $ ls -r "/lib" | where (_.extension == "so") | maxBy "size"

Note that "size" can be tab completed by Mash:

	/ $ ls -r "/lib" | where (_.extension == "so") | maxBy siz▶

### Task 2: Summarising file types

Suppose we want to tabulate the different types of file (as determined by file extension)
within a Python installation.

	/ $ cd "/usr/lib/python2.7"

We can use the `groupBy` function:

	/usr/lib/python2.7 $ ls -r | groupBy "extension"
	╔════════╤═════╤═══════════════════════════════════════════════════╗
	║key     │count│values                                             ║
	╟────────┼─────┼───────────────────────────────────────────────────╢
	║pyc     │1082 │new.pyc, pyclbr.pyc, DocXMLRPCServer.pyc, re.pyc, …║
	║so      │57   │dist-packages/bzrlib/_readdir_pyx.so, dist-package…║
	║egg-info│7    │argparse.egg-info, wsgiref.egg-info, dist-packages…║
	║doc     │1    │pdb.doc                                            ║
	║txt     │22   │LICENSE.txt, lib2to3/Grammar.txt, lib2to3/PatternG…║
	║py      │1083 │warnings.py, json/encoder.py, json/__init__.py, js…║
	╚════════╧═════╧═══════════════════════════════════════════════════╝

`groupBy` allows us group together items within a sequence by a common attribute. In this
case, we group by the `extension` method. For example, each file with a `py` extension is
placed within the same group; in this case, are 1083 such entries.

`groupBy` lets us add an additional `Total` group, including all items in the original
sequence, by adding the `--total` flag:

	/usr/lib/python2.7 $ ls -r | groupBy --total "extension"

We can also use the `select` function to pick members from the objects. This lets us prune
and rename columns:

	/usr/lib/python2.7 $ ls -r | groupBy --total "extension" | select --extension="key" "count"

Here, the `--extension="key"` argument creates a new field in the output objects, copied
from the `key` field in the input. The `"count"` argument simply copies the `count` field
across to the output unchanged.

Finally, we can sort our output by the `count` field:

	/usr/lib/python2.7 $ ls -r | groupBy --total "extension" | select --extension="key" "count" | sortBy "count"
	╔═════════╤═════╗
	║extension│count║
	╟─────────┼─────╢
	║doc      │1    ║
	║egg-info │7    ║
	║txt      │22   ║
	║so       │57   ║
	║pyc      │1082 ║
	║py       │1083 ║
	║Total    │2335 ║
	╚═════════╧═════╝

## Getting help

Documentation on functions, methods and fields can be displayed by using the `?` operator
after the item:

    / $ ls?
    NAME
            os.ls - List files
    
    CALLING SYNTAX
            ls <paths>... (--all | -a) (--recursive | -r) (--directory | -d)
    
    DESCRIPTION
            List files and directories, returning a sequence of PathSummary objects. 
            If no paths are supplied, the current directory is used as the default.
    
    PARAMETERS
    
            paths [variadic, optional] - Paths to list files
                    Paths can either be strings or PathSummary objects. 
                    If a given path is a file, it will be included in the output. 
                    If a given path is a directory, its children will be included, unless the
                       directory parameter is true, then it will be included directly. 
                    If no paths are provided, the default is the current working directory.
    
            --all | -a [optional] - Include files starting with a dot (default false)
    
            --recursive | -r [optional] - Recursively retrieve results from directories (default false)
    
            --directory | -d [optional] - List directories themselves, not their contents (default false)
    
Contextual assistance showing the parameters and flags for a function is displayed by
pressing Ctrl+Space:

	/ $ ls | groupBy 
	┌─ groupBy ──────────────────────────────────────────────────────────────────────────────┐
	│ Group together the elements of a sequence sharing a common key                         │
	│                                                                                        │
	│ groupBy <discriminator> <sequence> (--total[=<key>] | -t) (--includeNull[=<key>] | -n) │
	└────────────────────────────────────────────────────────────────────────────────────────┘
