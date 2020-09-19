import 'hello';

const paren = (require("lodash"));
const list = [require("underscore"), 0];
const obj = { debounce: require("debounce"), x() {} };
var regex = /[a-z]+/g + `x`;
const lazy = function*() {
  yield require("assert")
  throw new Error("Code should be unreachable");
  (function* gen() { yield 1; })();
}
with(b) for(;;);
for (var x in [1,2,3]) {
  if (x == 1) continue
  break;
}
