const paren = (require("lodash"));
const list = [require("underscore"), 0];
const obj = { debounce: require("debounce") };
const lazy = function*() {
  yield require("assert")
  throw new Error("Code should be unreachable");
}
