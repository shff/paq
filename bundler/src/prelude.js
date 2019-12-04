(function() {
  var __deps = {};
  var __req = (self) => {
    const require = (m) => require._module(m).exports;
    require._deps = {};
    require._module = (m) => {
      let fn = self ? __deps[require._deps[m]] : __deps[m];
      if (fn.module) return fn.module;
      const module = { exports: {} };
      fn.module = module;
      module.require = __req(module);
      module.require._deps = fn.deps;
      fn.func(module, module.exports, module.require);
      return module;
    };
    return require;
  };
