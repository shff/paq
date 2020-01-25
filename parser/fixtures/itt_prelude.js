'use strict'

function id(x) {return x}
function is(xs) {return typeof xs[Symbol.iterator] === 'function' || typeof xs.next === 'function'}
function generator(gen) {return (...args) => new Iter(gen(...args))}
const G = generator
function toRaw(iter) {return iter[Symbol.iterator] ? iter[Symbol.iterator]() : iter}
function from(iter) {return new Iter(toRaw(iter))}
const empty = G(function*() {})
