#!/usr/bin/env bash
# Build Mash for the browser into www/pkg. Run inside the Nix dev shell (for wasm-bindgen, wasm-opt and node):
#
#   nix develop -c rust/crates/mash-web/build.sh [--dev]
#
# Then serve www/: python3 rust/crates/mash-web/www/serve.py 8000 rust/crates/mash-web/www
set -euo pipefail
cd "$(dirname "$0")"

profile=release
if [[ "${1:-}" == "--dev" ]]; then profile=dev; fi
target_dir=$(cargo metadata --format-version 1 --no-deps | node -e 'process.stdout.write(JSON.parse(require("fs").readFileSync(0)).target_directory)')

cargo build --target wasm32-unknown-unknown -p mash-web --profile "$profile"
out=$([[ $profile == dev ]] && echo debug || echo release)
wasm-bindgen --target web --no-typescript --out-dir www/pkg "$target_dir/wasm32-unknown-unknown/$out/mash_web.wasm"
if [[ $profile == release ]]; then
  wasm-opt -O2 --enable-bulk-memory --enable-nontrapping-float-to-int --enable-sign-ext --enable-mutable-globals \
    www/pkg/mash_web_bg.wasm -o www/pkg/mash_web_bg.wasm
fi
ls -l www/pkg/mash_web_bg.wasm
