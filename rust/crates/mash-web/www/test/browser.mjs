// Checks the browser build in real browsers (Chromium, Firefox and WebKit, headless, via Playwright): that deep
// recursion stops with Mash's error before the browser's stack overflows, and how deep each browser allows.
// Run after build.sh, with Playwright (playwright-core and its browsers) on NODE_PATH:
//
//   NODE_PATH=/path/to/node_modules node rust/crates/mash-web/www/test/browser.mjs [ENGINE …] [-- PROGRAM …]
//
// ENGINE is chromium, firefox or webkit (default: all); PROGRAM is a name in recursion.mjs (default: all).

import { createRequire } from 'node:module';
import { RECURSION } from './recursion.mjs';
import { serve, stop } from './server.mjs';

const require = createRequire(import.meta.url);
const playwright = require('playwright-core');

const PORT = 8123;

function run(page, source, probe) {
  return page.evaluate(([s, p]) => window.runInWorker(s, p), [source, probe]);
}

/** Thrown when Mash can't be run at all, so there's no point going on */
class Broken extends Error {}

/** The deepest recursion that doesn't overflow, without the stack check (each try in a fresh worker) */
async function overflowDepth(page, program) {
  let low = 1;
  let high = 16384;
  while (high - low > 1) {
    const middle = Math.floor((low + high) / 2);
    const result = await run(page, `${program} ${middle}`, false);
    if (result.failed || result.timeout) throw new Broken(result.failed ?? `timed out at depth ${middle}`);
    if (result.value !== undefined) low = middle;
    else high = middle;
  }
  return low;
}

const args = process.argv.slice(2);
const split = args.includes('--') ? args.indexOf('--') : args.length;
const engines = split ? args.slice(0, split) : ['chromium', 'firefox', 'webkit'];
const programs = args.slice(split + 1).length ? args.slice(split + 1) : Object.keys(RECURSION);

let failures = 0;
const server = await serve(PORT);
try {
  for (const engine of engines) {
    const browser = await playwright[engine].launch();
    try {
      const page = await browser.newPage();
      page.on('pageerror', (error) => console.log(`  page error: ${error}`));
      await page.goto(`http://localhost:${PORT}/test/depth.html`);
      await page.waitForFunction(() => window.ready, null, { timeout: 30000 });
      console.log(`${engine} ${browser.version()}`);
      for (const name of programs) {
        const program = RECURSION[name];
        const started = Date.now();
        const checked = await run(page, `${program} 1000000`, true);
        if (checked.failed || checked.timeout) throw new Broken(checked.failed ?? 'timed out');
        const stopped = /Maximum call depth \((\d+)\) exceeded/.exec(checked.error ?? '');
        if (!stopped) {
          failures++;
          console.log(`  FAIL ${name}: ${checked.error ?? checked.value}`);
          continue;
        }
        const fits = await overflowDepth(page, program);
        // Mash's depth counts calls: for map and sortBy, two per level of recursion (the function and the lambda)
        console.log(
          `  ${name.padEnd(7)} stops cleanly at depth ${stopped[1]}; without the check, ${fits} levels fit ` +
            `(${((Date.now() - started) / 1000).toFixed(1)} s)`,
        );
      }
    } catch (error) {
      failures++;
      console.log(`  FAIL ${engine}: ${error.message}`);
    } finally {
      await browser.close();
    }
  }
} finally {
  stop(server);
}
process.exitCode = failures ? 1 : 0;
