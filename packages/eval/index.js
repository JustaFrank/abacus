const Sval = require('sval')

function evaluate(input) {
  const interpreter = new Sval({ ecmaVer: 9, sandBox: true })
  interpreter.run(`const result=${input};exports.result=result`)
  return interpreter.exports.result
}

module.exports = { evaluate }
