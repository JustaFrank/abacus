// Links compiled Purescript world to Javascript

const { Just, Nothing } = require('./output/Data.Maybe')
const { calculate: psCalculate } = require('./output/Main/index')
const { Func } = require('./output/Abacus.Expr.Token')

let state

function calculate(str) {
  // console.log(state)
  const res = psCalculate(state || { vars: [], funcs: [], opers: [] })(str)
    .value0
  if (res && res.env) state = res.env
  return res.result || res
}

// function toPsFunction(func) {
//   const arity = func.length
//   const symbol = func.name
//   const exec = args => Just.create(func(...args))
//   return Func({ arity, symbol, exec })
// }

module.exports = { calculate }
