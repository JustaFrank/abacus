const Sval = require('sval')

function evaluate(input, imports = {}) {
  const interpreter = new Sval({ ecmaVer: 9, sandBox: true })
  interpreter.import(imports)
  interpreter.run(`const result=${input};exports.result=result`)
  return interpreter.exports.result
}

module.exports = { evaluate }
