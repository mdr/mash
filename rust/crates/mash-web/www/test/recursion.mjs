// Recursive Mash programs for the tests, each taking the depth as its last argument

export const RECURSION = {
  plain: 'def f n = if n == 0 then 0 else 0 + f (n - 1); f',
  map: 'def g n = if n == 0 then 0 else [n] | map (x => g (x - 1)) | first; g',
  method: 'class C { def m n = if n == 0 then 0 else this.m (n - 1) }; C.new.m',
  sortBy: "def s n = if n == 0 then 0 else [n] | sortBy (x => s (x - 1)) | first; s",
  block: 'def b n = { x = n - 1; if x < 0 then 0 else { y = b x; y } }; b',
};

