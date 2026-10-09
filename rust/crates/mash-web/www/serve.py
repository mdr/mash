"""Serve the page on http://localhost:8000 (or the port given), cross-origin isolated so that Ctrl-C can interrupt
a running command (the page shares memory with Mash's worker).

    python3 serve.py [PORT [DIRECTORY]] [--not-isolated]

--not-isolated leaves out the isolation headers, as many hosts do (for testing the page without them)."""

import http.server
import sys


class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {
        **http.server.SimpleHTTPRequestHandler.extensions_map,
        ".js": "text/javascript",
        ".wasm": "application/wasm",
    }

    isolated = True

    def end_headers(self):
        if self.isolated:
            self.send_header("Cross-Origin-Opener-Policy", "same-origin")
            self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        self.send_header("Cache-Control", "no-store")
        super().end_headers()


if __name__ == "__main__":
    args = [a for a in sys.argv[1:] if a != "--not-isolated"]
    Handler.isolated = "--not-isolated" not in sys.argv[1:]
    port = int(args[0]) if args else 8000
    directory = args[1] if len(args) > 1 else "."
    server = http.server.ThreadingHTTPServer(
        ("127.0.0.1", port), lambda *args: Handler(*args, directory=directory)
    )
    print(f"Serving Mash on http://localhost:{port}/", flush=True)
    server.serve_forever()
