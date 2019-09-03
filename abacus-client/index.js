const readline = require('readline')

const { calculate } = require('abacus-calculator')

const options = {
  functions: [
    function pythagorean(a, b) {
      return Math.sqrt(a * a + b * b)
    }
  ]
}

const rl = readline.createInterface(process.stdin, process.stdout)

rl.setPrompt('abacus> ')
rl.prompt()
rl.on('line', function(line) {
  if (line === 'quit') rl.close()
  console.log(calculate(line, options))
  rl.prompt()
}).on('close', function() {
  process.exit(0)
})
