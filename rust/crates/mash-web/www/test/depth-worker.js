// Evaluates Mash code for depth.html, with or without the native stack check. Posts back `value`, `error` (an
// exception from Mash: its error, or a stack overflow) or `failed` (Mash couldn't be loaded).
import init, { evaluate } from '../pkg/mash_web.js';
import { stackNearlyFull } from '../stack.js';

onmessage = async ({ data }) => {
  try {
    await init({ module_or_path: data.module });
  } catch (error) {
    postMessage({ failed: `could not load Mash: ${error}` });
    return;
  }
  const bridge = {
    write() {},
    nowMillis: () => Date.now(),
    timeZone: () => 'UTC',
    randomU32: () => 4,
    interruptRequested: () => false,
    stackNearlyFull: data.probe ? stackNearlyFull : () => false,
    sleep() {},
    commandRunning() {},
    saveHistory() {},
    setClipboard() {},
    openUrl() {},
    httpRequest() {
      throw new Error('no network');
    },
  };
  try {
    postMessage({ value: evaluate(bridge, data.source) });
  } catch (error) {
    postMessage({ error: String(error) });
  }
};
