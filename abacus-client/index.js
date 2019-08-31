const { char, string, test } = require('abacus-calculator')

console.log(char('a')('asdf'))
console.log(string('asdf')('asdflksdjf'))
console.log(JSON.stringify(test('1+3-4*(234.2134 / (3 * 4 ^ 3))'), null, 4))
