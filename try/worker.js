// Runs Mash in a Web Worker, so that a long command doesn't freeze the page. Messages from the page (main.js):
//   { type: 'init', rows, columns, history: [json, …], interrupt: Int32Array over a SharedArrayBuffer, or null,
//     pageUrl, replay: [command, …] (from a shared link, to run in turn) }
//   { type: 'key', key, code, ctrl, alt, shift, meta }, { type: 'paste', text }, { type: 'resize', rows, columns },
//   { type: 'cancelReplay' }
// Messages to the page:
//   { type: 'ready' }, { type: 'output', text }, { type: 'running', running }, { type: 'history', entry },
//   { type: 'clipboard', text }, { type: 'open', url }, { type: 'exit' }, { type: 'crash', message }

import init, { WebShell, lastPanic } from './pkg/mash_web.js';
import { stackNearlyFull } from './stack.js';

let shell = null;
// interrupt[0] is set to 1 by the page on Ctrl-C while a command runs (if the page can share memory)
let interrupt = null;
let output = '';
let lastFlush = 0;
// Commands from a shared link still to run, and the timer that runs the next
let replay = [];
let replayTimer = null;
// A pause between replayed commands, so that each can be seen
const REPLAY_PAUSE_MS = 150;

function flush() {
  if (output) {
    postMessage({ type: 'output', text: output });
    output = '';
  }
  lastFlush = Date.now();
}

/** Lines of `name: value` headers as [name, value, name, value, …] */
function parseHeaders(text) {
  const flat = [];
  for (const line of text.split(/\r?\n/)) {
    const colon = line.indexOf(':');
    if (colon > 0) flat.push(line.slice(0, colon).trim(), line.slice(colon + 1).trim());
  }
  return flat;
}

// What Mash needs from JavaScript (mash-web's `Bridge`)
const bridge = {
  write(text) {
    output += text;
    // While a command runs, send its output now and then, so that the page shows progress
    if (output.length > 65536 || Date.now() - lastFlush > 50) flush();
  },
  nowMillis: () => Date.now(),
  timeZone() {
    try {
      return Intl.DateTimeFormat().resolvedOptions().timeZone;
    } catch {
      return undefined;
    }
  },
  randomU32: () => crypto.getRandomValues(new Uint32Array(1))[0],
  interruptRequested: () => (interrupt ? Atomics.exchange(interrupt, 0, 0) === 1 : false),
  stackNearlyFull,
  sleep(millis) {
    flush();
    if (interrupt) {
      // Wakes early when the page sets the flag and notifies
      Atomics.wait(interrupt, 0, 0, millis);
    } else {
      const end = performance.now() + millis;
      while (performance.now() < end) {
        // No way to block without shared memory
      }
    }
  },
  commandRunning(running) {
    flush();
    postMessage({ type: 'running', running });
  },
  saveHistory(entry) {
    postMessage({ type: 'history', entry });
  },
  setClipboard(text) {
    postMessage({ type: 'clipboard', text });
  },
  openUrl(url) {
    postMessage({ type: 'open', url });
  },
  // Synchronous XMLHttpRequest is allowed in workers. The browser follows redirects, and refuses responses from
  // sites that don't allow requests from this page (CORS).
  httpRequest(method, url, headers, body) {
    const request = new XMLHttpRequest();
    request.open(method, url, false);
    request.responseType = 'arraybuffer';
    for (let i = 0; i + 1 < headers.length; i += 2) {
      try {
        request.setRequestHeader(headers[i], headers[i + 1]);
      } catch {
        // A header the browser doesn't let pages set
      }
    }
    let sent = true;
    try {
      request.send(body ?? null);
    } catch {
      sent = false;
    }
    if (!sent || request.status === 0) {
      throw new Error(`Could not get ${url}: the site can't be reached, or doesn't allow requests from this page (CORS)`);
    }
    return {
      status: request.status,
      headers: parseHeaders(request.getAllResponseHeaders()),
      body: new Uint8Array(request.response ?? new ArrayBuffer(0)),
    };
  },
};

/** Handle a message from the page. Returns false once the user has left the shell. */
function handle(message) {
  switch (message.type) {
    case 'key':
    case 'paste': {
      const browsing = shell.inObjectBrowser();
      const alive =
        message.type === 'key'
          ? shell.key(message.key, message.code, message.ctrl, message.alt, message.shift, message.meta)
          : shell.paste(message.text);
      // While replaying, keys may be for the object browser (closing it continues the replay); anything else
      // means the user has taken over
      if (replay.length && !shell.inObjectBrowser()) {
        if (browsing && shell.atPrompt()) scheduleReplay();
        else replay = [];
      }
      return alive;
    }
    case 'resize':
      return shell.resize(message.rows, message.columns);
    case 'cancelReplay':
      replay = [];
      return true;
  }
  return true;
}

function scheduleReplay() {
  if (replay.length && replayTimer === null) replayTimer = setTimeout(replayNext, REPLAY_PAUSE_MS);
}

/** Run the next replayed command, if Mash is waiting for one (after the object browser, it waits for a key) */
function replayNext() {
  replayTimer = null;
  if (!replay.length || !shell.atPrompt()) return;
  guarded(() => shell.runCommand(replay.shift()));
  scheduleReplay();
}

/** Do something with the shell (returning false once the user has left), reporting the end of the session */
function guarded(action) {
  try {
    if (!action()) {
      replay = [];
      flush();
      postMessage({ type: 'exit' });
    }
    flush();
  } catch (error) {
    replay = [];
    flush();
    let reason = String(error);
    try {
      reason = lastPanic() ?? reason;
    } catch {
      // Mash is in no state to answer
    }
    postMessage({ type: 'crash', message: reason });
  }
}

onmessage = async (event) => {
  const message = event.data;
  if (message.type === 'init') {
    try {
      await init({ module_or_path: new URL('./pkg/mash_web_bg.wasm', import.meta.url) });
    } catch (error) {
      postMessage({ type: 'crash', message: String(error) });
      return;
    }
    interrupt = message.interrupt;
    guarded(() => {
      shell = new WebShell(bridge, message.rows, message.columns, message.history);
      shell.setPageUrl(message.pageUrl);
      postMessage({ type: 'ready' });
      shell.start();
      return true;
    });
    replay = message.replay ?? [];
    scheduleReplay();
  } else {
    guarded(() => handle(message));
  }
};
