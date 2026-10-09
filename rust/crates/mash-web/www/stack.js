// Whether the native stack, on which wasm runs, is nearly full (mash-web's `Bridge::stack_nearly_full`).
//
// Overflowing it while Mash is running throws a RangeError through Mash's frames, leaving Mash unusable. So
// Mash asks first, every few nested calls once it's deep: calling a function with N arguments pushes them onto
// the stack, and if they don't fit, the engine throws here, in JavaScript, where catching it is harmless.

/** Stack to keep free for Mash's frames between checks, and for reporting the error */
const RESERVE_BYTES = 64 * 1024;
const ARGUMENTS = new Array(RESERVE_BYTES / 8).fill(0);

function probe() {
  return arguments.length;
}

export function stackNearlyFull() {
  try {
    Reflect.apply(probe, undefined, ARGUMENTS);
    return false;
  } catch {
    return true;
  }
}
