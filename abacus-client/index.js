const { calculate } = require('abacus-calculator')

console.log(calculate({ funcs: [], opers: [], useDefFuncs: true, useDefOpers: true })("1+3"))

