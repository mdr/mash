Mash is an experimental object shell for Unix.

Website: http://mash-shell.org

## Try it

In the browser: https://mash-shell.org/try/

With [Nix](https://nixos.org/) (flakes enabled), run the Rust port of Mash, built from this repo:

    nix run github:mdr/mash

The first run compiles it, which takes a few minutes. Use `nix profile install github:mdr/mash` to keep it.

[![Mash chat room on Gitter](https://badges.gitter.im/mdr/mash.svg)](https://gitter.im/mdr/mash?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Development

Mash is built with [sbt](http://www.scala-sbt.org/) and needs a JDK (17 recommended; newer JDKs work
but print warnings about native access from jnr/jffi).

### With Nix (recommended)

The repo includes a Nix flake providing a pinned JDK 17 and sbt:

    nix develop

Or, with [direnv](https://direnv.net/), run `direnv allow` once and the environment loads
automatically on `cd`. To update the pinned toolchain, run `nix flake update`.

### Without Nix

On macOS:

    brew install openjdk@17 sbt
    export JAVA_HOME=/opt/homebrew/opt/openjdk@17

### Building and running

Run the tests:

    sbt test

To run the shell, build a launcher script and run it in a terminal:

    sbt stage
    target/universal/stage/bin/mash
