const readline = require('readline')

const { calculate } = require('abacus-calculator')

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
})

rl.setPrompt('>> ')
rl.prompt()

rl.on('line', function(line) {
  switch (line.trim()) {
    case ':q':
      rl.close()
      break
    case 'clear':
      console.log('\033[2J')
      console.log('\033c')
      break
    default:
      console.log(calculate(line))
      break
  }
  rl.prompt()
}).on('close', function() {
  process.exit(0)
})
