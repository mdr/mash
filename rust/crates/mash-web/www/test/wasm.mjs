// Checks the wasm build under Node (no browser): evaluation, the time zone database, interrupts, and that
// recursion up to the call depth limit fits the stack. Run after build.sh (use --dev to check a debug build,
// whose frames are largest):
//
//   node rust/crates/mash-web/www/test/wasm.mjs

import { readFileSync } from 'node:fs';
import assert from 'node:assert/strict';
import { initSync, evaluate } from '../pkg/mash_web.js';
import { stackNearlyFull } from '../stack.js';
import { RECURSION } from './recursion.mjs';

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
    stackNearlyFull,
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
  recursionStopsCleanlyBeforeTheStackRunsOut() {
    for (const [name, program] of Object.entries(RECURSION)) {
      const error = run(`${program} 1000000`);
      assert.match(error, /Maximum call depth \(\d+\) exceeded/, `${name}: ${error}`);
      console.log(`     ${name}: stopped at depth ${error.match(/\((\d+)\)/)[1]}`);
      // Mash is still usable
      assert.equal(run('[1, 2] | map (_ + 1)'), '[2, 3]', name);
    }
  },
  moderateRecursionWorks() {
    assert.equal(run(`${RECURSION.plain} 200`), '0');
    assert.equal(run(`${RECURSION.map} 60`), '0');
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
