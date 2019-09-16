const readline = require('readline')

const Calculator = require('@abacus/calculator').default
const { evaluate } = require('@abacus/eval')

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
})

const calculator = new Calculator([
  evaluate(`function pythagorean(a, b) {
    return Math.sqrt(Math.pow(a, 2) + Math.pow(b, 2))
  }`)
])

rl.setPrompt('>> ')
rl.prompt()

rl.on('line', line => {
  switch (line.trim().toLowerCase()) {
    case ':q':
    case ':quit':
      rl.close()
      break
    case ':c':
    case ':clear':
      console.log('\u001b[2J\u001b[0;0H')
      break
    default:
      console.log(calculator.run(line))
      break
  }
  rl.prompt()
}).on('close', () => {
  process.exit(0)
})
