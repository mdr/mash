#!/usr/bin/env bash
# Copy the page, as built by build.sh, into a directory to publish (leaving out serve.py and the tests):
#
#   rust/crates/mash-web/site.sh DIRECTORY
#
# Check it with: python3 rust/crates/mash-web/www/serve.py 8000 DIRECTORY --not-isolated
set -euo pipefail
out=${1:?usage: site.sh DIRECTORY}
www="$(dirname "$0")/www"

if [[ ! -f $www/pkg/mash_web_bg.wasm ]]; then
  echo "site.sh: no build in $www/pkg; run build.sh first" >&2
  exit 1
fi
mkdir -p "$out/pkg"
cp "$www"/{index.html,main.js,worker.js,stack.js,coi-serviceworker.js} "$out/"
cp "$www"/pkg/{mash_web.js,mash_web_bg.wasm} "$out/pkg/"
