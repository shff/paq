(function() {
  var __req = self => dep => {
    let fn = self.deps[dep];
    if (fn.module) return fn.module.exports;
    fn.module = { exports: {}, require: __req(fn) };
    fn(fn.module, fn.module.exports, fn.module.require);
    return fn.module.exports;
  };
