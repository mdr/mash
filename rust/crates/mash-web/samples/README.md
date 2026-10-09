# Mash in the browser

This is Mash, an object shell, running entirely in your browser (compiled to WebAssembly).

The file system lives in memory: you can create, change and delete files, but they are gone when the page is
closed. Running programs, processes and disk space aren't available here; HTTP requests work for sites that
allow them (CORS).

Things to try:

    ls "examples"
    ls "examples" | sortBy (_.size) | reverse
    readLines "README.md" | grep "Mash"
    json.read "examples/planets.json" | sortBy (_.moons) | reverse
    json.read "examples/planets.json" | where (_.rings) | map (_.name)
    readLines "examples/sales.csv" | skip 1 | map (_.split ",") | groupBy first
    mash.source "examples/fib.mash"; fibs 20
    help ls

Type part of a name and press Tab to complete it.

Press Ctrl-C to interrupt a command. Large results open in the object browser: press q to leave it.
