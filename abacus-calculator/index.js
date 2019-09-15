// Links compiled Purescript world to Javascript

const { Func, toCompExec } = require('./output/Abacus.Expr.Token')
const { abacus } = require('./output/Abacus/index')
const { StateT } = require('./output/Control.Monad.State')
const { Just, Nothing } = require('./output/Data.Maybe')

class Calculator {
  constructor(funcs = []) {
    this.env = { vars: [], funcs: funcs.map(makeFunc), opers: [] }
  }

  run(str) {
    const { env, result } = abacus(this.env)(str)
    this.env = env
    return result
  }
}

function makeFunc(func) {
  const symbol = func.name
  const arity = func.length
  const exec = toCompExec(arity)(args => func(...args))
  return Func({ symbol, comp: { arity, exec } })
}

module.exports = Calculator
