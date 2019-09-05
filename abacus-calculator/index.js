// Links compiled Purescript world to Javascript

const { Just } = require('./output/Data.Maybe')
const { calculate: psCalculate } = require('./output/Main/index')
const { Func } = require('./output/Abacus.Expr.Token')

function calculate(str) {
  return psCalculate({
    funcs: [],
    opers: [],
    useDefFuncs: true,
    useDefOpers: true
  })(str).value0
}

// function toPsFunction(func) {
//   const arity = func.length
//   const symbol = func.name
//   const exec = args => Just.create(func(...args))
//   return Func({ arity, symbol, exec })
// }

module.exports = { calculate }
