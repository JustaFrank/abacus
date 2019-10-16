import Sval from 'sval'

export const evaluate = (
  input: string,
  imports:
    | string
    | {
        [name: string]: any
      } = {}
) => {
  const interpreter = new Sval({ ecmaVer: 9, sandBox: true })
  interpreter.import(imports)
  interpreter.run(`const result=${input};exports.result=result`)
  return interpreter.exports.result
}
