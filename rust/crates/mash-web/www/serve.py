"""Serve the page on http://localhost:8000 (or the port given), cross-origin isolated so that Ctrl-C can interrupt
a running command (the page shares memory with Mash's worker)."""

import http.server
import sys


class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {
        **http.server.SimpleHTTPRequestHandler.extensions_map,
        ".js": "text/javascript",
        ".wasm": "application/wasm",
    }

    def end_headers(self):
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        self.send_header("Cache-Control", "no-store")
        super().end_headers()


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8000
    directory = sys.argv[2] if len(sys.argv) > 2 else "."
    server = http.server.ThreadingHTTPServer(
        ("127.0.0.1", port), lambda *args: Handler(*args, directory=directory)
    )
    print(f"Serving Mash on http://localhost:{port}/", flush=True)
    server.serve_forever()
