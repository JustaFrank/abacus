const readline = require('readline')

const { calculate } = require('abacus-calculator')

const options = {
  functions: [
    function pythagorean(a, b) {
      return Math.sqrt(a * a + b * b)
    }
  ]
}

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
})

rl.setPrompt('abacus> ')
rl.prompt()

rl.on('line', function(line) {
  switch (line.trim()) {
    case ':q':
      rl.close()
      break
    default:
      console.log(calculate(line, options))
      break
  }
  rl.prompt()
}).on('close', function() {
  process.exit(0)
})
