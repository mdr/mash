---
layout: default
title: Mash FAQ
permalink: /faq/
---

# Mash FAQ

## What is Mash?

Mash is a shell for Linux that uses objects, rather than text, as its fundamental data
model. It is open source under the [MIT Licence](https://opensource.org/licenses/MIT).

Mash is currently considered experimental, and the existing implementation is a proof of
concept. Feedback is very welcome, as would be collaborators in taking Mash
forward.

Mash is initially targeted at Linux, but other Unix-like systems may be supported in the
future.

## Why another shell?

**TLDR**: to support objects, because objects are awesome.

Modern Unix shells, such as [bash](https://www.gnu.org/software/bash/),
[zsh](http://www.zsh.org/) and [fish](http://fishshell.com/), implement sophisticated shell
languages and provide rich interactivity features. They are fundamentally **text-based**
&mdash; commands work together by consuming and emitting character data through
pipes. Powerful generic text search and manipulation utilities, such as `grep`, `sed` and
`awk`, can be used to transform data as required. As Doug McIlroy put it when advocating for
the Unix philosophy, *"write programs to handle text streams, because that is a universal
interface."*

By contrast, Mash is **object-based** &mdash; commands consume and emit objects, which have
fields and methods. Why might using objects be preferable to text?

* Commands that process text can quickly become cryptic and hard to read and write. Regular
  expressions, for example, are notoriously prone to being obscure. As another example,
  understanding `sort -nrk 7` requires knowing what data was held in column 7. An object
  shell can let you work at a higher level of abstraction: in Mash you'd just write `sortBy
  "size"`. This lets you focus on the task you want to achieve, rather than munging text.
* Text output can be hard to handle robustly. Many Unix tools will by default use newlines
  or other whitespace to delimit data; however, the data itself might contain newlines and
  whitespace, and without care this can easily cause commands and scripts to break. The use
  of objects creates a clean separation between data and its structure; nothing special is
  required to handle newlines or whitespace.
* The output format of commands vary, and isn't always obvious: what does the second column
  in the output of `ls -l` signify? In Mash, fields are labelled with descriptive
  identifiers, and can be easily introspected to provide additional user documentation.
* Existing shells don't provide any interactive assistance for extracting information out of
  the output of commands. Mash is able provide completions and other assistance on result
  objects. With type inference, this can happen even before execution.

## Why not just port Powershell?

PowerShell is an object shell for Microsoft Windows. While their designs differ in many
ways, both PowerShell and Mash share the philosophy that objects should be the fundamental
medium for data, rather than text.

There is an effort to port PowerShell to Unix
([pash](https://github.com/Pash-Project/Pash)).

PowerShell is designed for Windows, and as such, does not cover many key features in Unix
(and many PowerShell features have no applicability in Unix). Mash is designed for Linux
from the outset.

## Why not a full-blown programming language?

Programming languages are designed for developing applications, rather than as a interactive
environment for system administration. Some languages do support powerful REPLs (IPython
being a particularly noteworthy example), however, performing system tasks using programming
libraries is rarely as straightforward as the comparable operations in the shell, and REPLs
lacks many interactivity features such as file path completion.

There are projects aimed at implementing shells on top of programming languages. In
particular:

* [Xonsh](http://xonsh.org): Python
* [Ammonite](http://lihaoyi.github.io/Ammonite/): Scala

## How will Mash work if everything else is text-based?

Most existing command-line programs are text-based. If Mash only produces and consumes
objects, this might be a fundamental obstacle.

The idea is to take two approaches:

* Develop wrapper libraries for the most widely-used tools and utilities. For example,
  working with filesystems, processes, users/groups, and common applications like Git,
  Mercurial, etc. While it would be unfeasible to wrap every tool and utility that exists,
  it seems plausible to wrap enough that the majority of day-to-day tasks are covered.
* Mash has a subsyntax for more traditional shell-like command execution called *Mish*. Mash
  expressions can embed Mish fragments, and vice versa. In this way, existing command-line
  programs that aren't yet wrapped can be executed more-or-less as in existing shells.
