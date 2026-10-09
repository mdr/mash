// Checks the wasm build under Node (no browser): evaluation, the time zone database, interrupts, and that
// recursion up to the call depth limit fits the stack. Run after build.sh (use --dev to check a debug build,
// whose frames are largest):
//
//   node rust/crates/mash-web/www/test/wasm.mjs

import { readFileSync } from 'node:fs';
import assert from 'node:assert/strict';
import { initSync, evaluate } from '../pkg/mash_web.js';

initSync({ module: readFileSync(new URL('../pkg/mash_web_bg.wasm', import.meta.url)) });

function bridge(overrides = {}) {
  let output = '';
  return {
    write: (text) => (output += text),
    output: () => output,
    nowMillis: () => Date.parse('2026-10-09T12:00:00Z'),
    timeZone: () => 'Asia/Tokyo',
    randomU32: () => 4,
    interruptRequested: () => false,
    sleep: () => {},
    commandRunning: () => {},
    saveHistory: () => {},
    setClipboard: () => {},
    openUrl: () => {},
    httpRequest: () => {
      throw new Error('no network in this test');
    },
    ...overrides,
  };
}

function run(source, b = bridge()) {
  try {
    return evaluate(b, source);
  } catch (error) {
    return `error: ${error}`;
  }
}

const tests = {
  evaluates() {
    assert.equal(run('[1, 2, 3] | map (_ * 2) | sum'), '12');
    assert.equal(run("json.read 'examples/planets.json' | maxBy (_.moons) | (_.name)"), 'Saturn');
  },
  printsThroughTheBridge() {
    const b = bridge();
    run("print 'hello'", b);
    assert.equal(b.output(), 'hello\r\n');
  },
  usesTheBundledTimeZoneDatabase() {
    assert.equal(run('now.hour'), '21');
  },
  canBeInterrupted() {
    let polls = 0;
    const b = bridge({ interruptRequested: () => ++polls === 2 });
    assert.match(run('1.to 1000000 | map (n => n + 1) | sum', b), /nterrupted/);
  },
  failsAtUnhelpfulHttp() {
    assert.match(run("http.get 'https://example.com'"), /no network in this test/);
  },
  recursionToTheDepthLimitFitsTheStack() {
    const depth = 'def f n = if n == 0 then 0 else 1 + f (n - 1)';
    assert.equal(run(`${depth}; f 3990`), '3990');
    assert.match(run(`${depth}; f 100000`), /depth/);
    // Deeper Rust frames per Mash call: a method, a lambda and a pipeline in each
    const busy = 'def g n = if n == 0 then 0 else [n] | map (x => g (x - 1)) | first';
    assert.equal(run(`${busy}; g 1300`), '0');
    assert.match(run(`${busy}; g 100000`), /depth/);
  },
};

let failed = 0;
for (const [name, test] of Object.entries(tests)) {
  try {
    test();
    console.log(`ok   ${name}`);
  } catch (error) {
    failed++;
    console.log(`FAIL ${name}\n${error.message}`);
  }
}
process.exit(failed ? 1 : 0);
