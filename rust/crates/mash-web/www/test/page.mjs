// Checks the page itself in real browsers (headless, via Playwright): typing commands, Ctrl-C, history kept
// across reloads, the object browser, leaving and restarting. Run after build.sh, with Playwright
// (playwright-core and its browsers) on NODE_PATH, and network access (xterm.js comes from a CDN):
//
//   NODE_PATH=/path/to/node_modules node rust/crates/mash-web/www/test/page.mjs [chromium|firefox|webkit …]
//
// MASH_WWW=dir tests the page in dir instead of www/ (e.g. the site assembled by site.sh).

import assert from 'node:assert/strict';
import { createRequire } from 'node:module';
import { serve, stop } from './server.mjs';

const require = createRequire(import.meta.url);
const playwright = require('playwright-core');

const PORT = 8125;
/** Serves the page without cross-origin isolation */
const PLAIN_PORT = 8126;
const TIMEOUT_MS = 20000;

/** The lines on the terminal's screen (not the scrollback) */
function screen(page) {
  return page.evaluate(() => {
    const term = window.mashTerminal;
    const buffer = term.buffer.active;
    const lines = [];
    for (let i = 0; i < term.rows; i++) lines.push(buffer.getLine(buffer.viewportY + i)?.translateToString(true) ?? '');
    return lines.join('\n').trimEnd();
  });
}

/** Wait until the screen matches `pattern` */
async function waitFor(page, pattern, what = String(pattern)) {
  const deadline = Date.now() + TIMEOUT_MS;
  let text = '';
  while (Date.now() < deadline) {
    text = await screen(page);
    const lastLine = text.trimEnd().split('\n').at(-1);
    if (pattern.test(text) || pattern.test(lastLine)) return text;
    await page.waitForTimeout(50);
  }
  throw new Error(`timed out waiting for ${what}; the screen shows:\n${text}`);
}

/** The prompt for command number `n` (`[n] ~ $`, then the assistance indicator), on the last line in use */
const prompt = (n) => new RegExp(`^\\[${n}\\] [^\\n]*\\$ +[◉○]?\\s*$`);

async function command(page, text, n) {
  await page.keyboard.type(text);
  await page.keyboard.press('Enter');
  return waitFor(page, prompt(n), `prompt ${n} after ${text}`);
}

/** Open the page in `browser` (or a browser context) */
async function open(browser, port = PORT) {
  const page = await browser.newPage();
  page.on('pageerror', (error) => console.log(`    page error: ${error}`));
  await page.goto(`http://localhost:${port}/`);
  await startTerminal(page);
  return page;
}

async function startTerminal(page) {
  await page.waitForFunction(() => window.mashTerminal, null, { timeout: TIMEOUT_MS });
  await waitFor(page, prompt(0), 'the first prompt');
}

/** Interrupt a computation and a sleep with Ctrl-C, and check that the session (and its variables) survived */
async function interruptKeepingVariables(page) {
  await command(page, 'kept = 42', 1);
  await page.keyboard.type('1.to 100000 | map (i => 1.to 1000 | map (j => i * j) | sum) | sum');
  await page.keyboard.press('Enter');
  await page.waitForTimeout(500);
  await page.keyboard.press('Control+c');
  let text = await waitFor(page, prompt(2), 'the prompt after interrupting');
  assert.match(text, /nterrupted/);
  // And sleeping
  await page.keyboard.type('time.sleep 60.seconds');
  await page.keyboard.press('Enter');
  await page.waitForTimeout(300);
  await page.keyboard.press('Control+c');
  await waitFor(page, prompt(3), 'the prompt after interrupting sleep');
  text = await command(page, 'kept', 4);
  assert.match(text, /^42$/m);
}

const tests = {
  async runsCommands(browser) {
    const page = await open(browser);
    assert.equal(await page.evaluate(() => self.crossOriginIsolated), true, 'cross-origin isolated');
    let text = await command(page, '[1, 2, 3] | map (_ * 10) | sum', 1);
    assert.match(text, /^60$/m);
    text = await command(page, 'json.read "examples/planets.json" | where (_.rings) | map (_.name)', 2);
    assert.match(text, /^Jupiter\nSaturn\nUranus\nNeptune$/m);
    text = await command(page, 'print "one`ntwo"', 3);
    assert.match(text, /^one\ntwo$/m);
  },

  async ctrlCInterruptsACommand(browser) {
    await interruptKeepingVariables(await open(browser));
  },

  // Hosts like GitHub Pages can't send the isolation headers; coi-serviceworker.js adds them, after a reload
  async withoutHeadersTheServiceWorkerIsolatesThePage(browser) {
    const context = await browser.newContext();
    const page = await context.newPage();
    page.on('pageerror', (error) => console.log(`    page error: ${error}`));
    await page.goto(`http://localhost:${PLAIN_PORT}/`);
    const deadline = Date.now() + TIMEOUT_MS;
    let isolated = false;
    while (!isolated && Date.now() < deadline) {
      // The page reloads itself once the service worker is ready, which can interrupt evaluating
      isolated = await page.evaluate(() => self.crossOriginIsolated).catch(() => false);
      if (!isolated) await page.waitForTimeout(100);
    }
    assert.equal(isolated, true, 'cross-origin isolated by the service worker');
    await startTerminal(page);
    await interruptKeepingVariables(page);
    await context.close();
  },

  async historyIsKeptAcrossReloads(browser) {
    const context = await browser.newContext();
    let page = await open(context);
    await command(page, '"remember me".length', 1);
    await page.close();
    page = await open(context);
    await page.keyboard.press('ArrowUp');
    await waitFor(page, /\$ "remember me"\.length/, 'the recalled command');
    await context.close();
  },

  async largeOutputOpensTheObjectBrowser(browser) {
    const page = await open(browser);
    await page.keyboard.type('1.to 200 | map (n => { n: n, square: n * n })');
    await page.keyboard.press('Enter');
    await page.waitForFunction(() => window.mashTerminal.buffer.active.type === 'alternate', null, {
      timeout: TIMEOUT_MS,
    });
    await waitFor(page, /square/, 'the object browser\'s table');
    await page.keyboard.press('q');
    await waitFor(page, prompt(1), 'the prompt after the browser');
    const text = await command(page, 'it.length', 2);
    assert.match(text, /^200$/m);
  },

  async withoutIsolationCtrlCRestartsMash(browser) {
    // Without service workers, as over plain HTTP (where the browser doesn't allow them). (Blocked, register()
    // gives coi-serviceworker.js no registration, so it logs a page error; over HTTP it doesn't try.)
    const context = await browser.newContext({ serviceWorkers: 'block' });
    const page = await open(context, PLAIN_PORT);
    assert.equal(await page.evaluate(() => self.crossOriginIsolated), false, 'not cross-origin isolated');
    await command(page, 'kept = 42', 1);
    await page.keyboard.type('1.to 100000 | map (i => 1.to 1000 | map (j => i * j) | sum) | sum');
    await page.keyboard.press('Enter');
    await page.waitForTimeout(500);
    await page.keyboard.press('Control+c');
    await waitFor(page, /Mash has been restarted/, 'the restart message');
    await waitFor(page, prompt(0), 'the first prompt of the new session');
    const text = await command(page, 'global.get "kept"', 1);
    assert.match(text, /^null$/m);
    await context.close();
  },

  async ctrlDLeavesAndAKeyStartsAgain(browser) {
    const page = await open(browser);
    await page.keyboard.press('Control+d');
    await waitFor(page, /Mash has exited/, 'the exit message');
    await page.keyboard.press('Enter');
    await waitFor(page, prompt(0), 'a new session');
  },
};

let failures = 0;
const engines = process.argv.slice(2).length ? process.argv.slice(2) : ['chromium', 'firefox', 'webkit'];
const directory = process.env.MASH_WWW;
const server = await serve(PORT, { directory });
const plainServer = await serve(PLAIN_PORT, { isolated: false, directory });
try {
  for (const engine of engines) {
    const browser = await playwright[engine].launch();
    console.log(`${engine} ${browser.version()}`);
    for (const [name, test] of Object.entries(tests)) {
      try {
        await test(browser);
        console.log(`  ok   ${name}`);
      } catch (error) {
        failures++;
        console.log(`  FAIL ${name}: ${error.message}`);
      }
    }
    await browser.close();
  }
} finally {
  stop(server);
  stop(plainServer);
}
process.exitCode = failures ? 1 : 0;
