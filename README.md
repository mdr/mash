Mash is an experimental object shell for Unix.

Website: http://mash-shell.org

[![Mash chat room on Gitter](https://badges.gitter.im/mdr/mash.svg)](https://gitter.im/mdr/mash?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Development

Mash is built with [sbt](http://www.scala-sbt.org/) and needs a JDK (17 recommended; newer JDKs work
but print warnings about native access from jnr/jffi). On macOS:

    brew install openjdk@17 sbt
    export JAVA_HOME=/opt/homebrew/opt/openjdk@17

Run the tests:

    cd mash
    sbt test

To run the shell, build a launcher script and run it in a terminal:

    sbt stage
    target/universal/stage/bin/mash
