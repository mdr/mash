// Runs serve.py for the browser tests

import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const www = fileURLToPath(new URL('..', import.meta.url));

/** Start the server on `port`, once it's listening. PYTHON (default python3) runs it. */
export async function serve(port, { isolated = true } = {}) {
  const args = [`${www}/serve.py`, String(port), www, ...(isolated ? [] : ['--not-isolated'])];
  const server = spawn(process.env.PYTHON ?? 'python3', args, {
    stdio: ['ignore', 'pipe', 'pipe'],
    // Its own process group, so that stopping it stops any process it starts (macOS's python3 is a launcher)
    detached: true,
  });
  let errors = '';
  server.stderr.on('data', (data) => (errors += data));
  await new Promise((resolve, reject) => {
    server.stdout.once('data', resolve);
    server.once('exit', (code) => reject(new Error(`the server stopped (${code}): ${errors.trim()}`)));
  });
  // Drain its request log
  server.stderr.removeAllListeners('data');
  server.stderr.resume();
  return server;
}

export function stop(server) {
  process.kill(-server.pid);
}
