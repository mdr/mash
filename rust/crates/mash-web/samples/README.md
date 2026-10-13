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

The bookshop's data is nested: books have authors, prices and stock in each branch; orders have customers with
addresses, and items. Read it into a variable, then explore:

    shop = json.read "examples/bookshop.json"
    shop.books.author.name | unique
    shop.books | map (_.hoist "author") | select "title" "name" "country"
    shop.books | where (_.stock.Edinburgh == 0) | map (_.title)
    (shop.books | flatMap (_.genres) | groupBy identity).transformValues length
    (shop.orders | groupBy (_.customer.address.country)).transformValues (_.id)
    shop.branches | flatMap (b => b.staff | map (name => { name, city: b.city }))
    maths.stats shop.books.pricePence

Join each item sold to its book, then total the orders (in pence):

    sold = shop.orders | flatMap (o => o.items | map (i => i + { order: o.id }))
    sold.innerJoin shop.books (s b => s.book == b.id) | select "order" "title" "quantity" "pricePence"
    (sold.innerJoin shop.books (s b => s.book == b.id) | groupBy (_.order)).transformValues (_.sumBy (x => x.quantity * x.pricePence))

Type part of a name and press Tab to complete it.

Press Ctrl-C to interrupt a command. Large results open in the object browser: press q to leave it.
