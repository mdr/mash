Mash is an experimental object shell for Linux.

See: http://mdr.github.io/mash/

## Hacking

Requires javasysmon.jar, which isn't available in a Maven repo:

    git clone https://github.com/jezhumble/javasysmon.git
	cd javasysmon/
	ant
	cp target/javasysmon.jar $mashDir/lib/

Mash is built with [sbt](http://www.scala-sbt.org/):

    cd $mashDir
    sbt run

