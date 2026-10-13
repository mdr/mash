// The page: an xterm.js terminal, with Mash running in a worker (worker.js). Key presses, pastes and resizes go
// to the worker; what it writes goes to the terminal.
//
// A link can carry commands to run, made by Mash's `share`: #run=<command>&run=<command>…, each percent-encoded.
// The page lists them and runs them (in turn, as if typed) only if the user presses Enter.

/* global Terminal, FitAddon, Unicode11Addon */

const HISTORY_KEY = 'mash.history';
const HISTORY_LIMIT = 2000;

const term = new Terminal({
  allowProposedApi: true,
  cursorBlink: true,
  fontFamily: '"JetBrains Mono", Menlo, Consolas, "DejaVu Sans Mono", monospace',
  fontSize: 14,
  scrollback: 5000,
  theme: { background: '#1b1d23' },
});
const fit = new FitAddon.FitAddon();
term.loadAddon(fit);
// Wide characters (emoji, CJK) as the REPL measures them
term.loadAddon(new Unicode11Addon.Unicode11Addon());
term.unicode.activeVersion = '11';
term.open(document.getElementById('terminal'));
// For tests (test/page.mjs), which read the screen
window.mashTerminal = term;
fit.fit();
term.focus();

// Ctrl-C reaches a busy worker through shared memory, which needs the page to be cross-origin isolated
const interrupt =
  self.crossOriginIsolated && typeof SharedArrayBuffer !== 'undefined'
    ? new Int32Array(new SharedArrayBuffer(4))
    : null;

let worker = null;
let ready = false;
let pending = [];
let running = false;
let exited = false;
// Commands from the link, shown and waiting for the user to accept (Enter) or decline (Esc)
let offered = null;

function loadHistory() {
  try {
    const entries = JSON.parse(localStorage.getItem(HISTORY_KEY) ?? '[]');
    return Array.isArray(entries) ? entries.filter((e) => typeof e === 'string') : [];
  } catch {
    return [];
  }
}

function saveHistory(entry) {
  try {
    const entries = [...loadHistory(), entry].slice(-HISTORY_LIMIT);
    localStorage.setItem(HISTORY_KEY, JSON.stringify(entries));
  } catch {
    // Storage is full or blocked: the history just isn't kept
  }
}

function notice(text) {
  term.write(`\r\n\x1b[33m${text}\x1b[0m\r\n`);
}

function send(message) {
  if (ready) worker.postMessage(message);
  else pending.push(message);
}

/** The commands in the page's link, if any */
function linkedCommands() {
  try {
    return new URLSearchParams(location.hash.slice(1)).getAll('run').filter((command) => command.trim());
  } catch {
    return [];
  }
}

/** `text` with control characters visible, so that it shows as it is (a link can't move the cursor or recolour) */
function visible(text) {
  return text.replace(/[\x00-\x09\x0b-\x1f\x7f-\x9f]/g, (c) => `\\u{${c.codePointAt(0).toString(16)}}`);
}

function offer(commands) {
  offered = commands;
  const count = commands.length === 1 ? 'one command' : `${commands.length} commands`;
  term.write(`\x1b[33mThis link runs ${count} in Mash:\x1b[0m\r\n\r\n`);
  for (const command of commands) {
    term.write(`  ${visible(command).split('\n').join('\r\n    ')}\r\n`);
  }
  const them = commands.length === 1 ? 'it' : 'them';
  term.write(`\r\n\x1b[33mPress Enter to run ${them}, or Esc to start without ${them}.\x1b[0m\r\n\r\n`);
}

/** Start a session, running `replay` (commands) once it's ready */
function start(replay = []) {
  worker?.terminate();
  ready = false;
  pending = [];
  running = false;
  exited = false;
  worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' });
  worker.onmessage = ({ data }) => {
    switch (data.type) {
      case 'ready':
        ready = true;
        for (const message of pending) worker.postMessage(message);
        pending = [];
        break;
      case 'output':
        term.write(data.text);
        break;
      case 'running':
        running = data.running;
        break;
      case 'history':
        saveHistory(data.entry);
        break;
      case 'clipboard':
        navigator.clipboard?.writeText(data.text).catch((error) => console.warn('Could not copy:', error));
        break;
      case 'open':
        window.open(data.url, '_blank', 'noopener');
        break;
      case 'exit':
        exited = true;
        notice('Mash has exited. Press any key to start a new session.');
        break;
      case 'crash':
        notice(`Mash stopped: ${data.message}\r\nStarting a new session…`);
        start();
        break;
    }
  };
  worker.onerror = (event) => {
    notice(`Mash could not start: ${event.message ?? 'the worker failed to load'}`);
  };
  worker.postMessage({
    type: 'init',
    rows: term.rows,
    columns: term.cols,
    history: loadHistory(),
    interrupt,
    pageUrl: location.href,
    replay,
  });
}

function interruptCommand() {
  if (interrupt) {
    Atomics.store(interrupt, 0, 1);
    Atomics.notify(interrupt, 0);
    // And don't go on to any more commands from a link
    send({ type: 'cancelReplay' });
  } else {
    // Without shared memory, the only way to stop a busy worker is to replace it
    term.write('^C');
    notice('This page cannot interrupt a running command, so Mash has been restarted (variables are lost).');
    start();
  }
}

const MODIFIER_KEYS = new Set(['Shift', 'Control', 'Alt', 'Meta', 'AltGraph', 'CapsLock', 'NumLock', 'Fn', 'OS']);

term.attachCustomKeyEventHandler((event) => {
  // Command-key shortcuts (copy, paste, …) and Ctrl-Shift-C and -V belong to the browser
  if (event.metaKey || (event.ctrlKey && event.shiftKey && ['C', 'V'].includes(event.key.toUpperCase()))) {
    return true;
  }
  // Text being composed (dead keys, input methods) arrives as data once complete
  if (event.isComposing || event.key === 'Dead' || event.keyCode === 229) return true;
  if (event.type !== 'keydown' || MODIFIER_KEYS.has(event.key)) return false;
  event.preventDefault();
  if (offered) {
    if (event.key === 'Enter' || event.key === 'Escape') {
      const commands = event.key === 'Enter' ? offered : [];
      offered = null;
      start(commands);
    }
    return false;
  }
  if (exited) {
    start();
    return false;
  }
  if (running && event.ctrlKey && event.key.toLowerCase() === 'c') {
    interruptCommand();
    return false;
  }
  send({
    type: 'key',
    key: event.key,
    code: event.code,
    ctrl: event.ctrlKey,
    alt: event.altKey,
    shift: event.shiftKey,
    meta: event.metaKey,
  });
  return false;
});

// Pastes and composed text. xterm.js sends a line break in a paste as a carriage return.
term.onData((data) => {
  if (offered || exited || data.startsWith('\x1b')) return;
  send({ type: 'paste', text: data.replace(/\r\n?/g, '\n') });
});

term.onResize(({ rows, cols }) => send({ type: 'resize', rows, columns: cols }));
new ResizeObserver(() => fit.fit()).observe(document.getElementById('terminal'));

const commands = linkedCommands();
if (commands.length) offer(commands);
else start();
