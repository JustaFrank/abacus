// Lots of `any` types because connecting Purescript to Typescript and I am
// lazy :)

import { Func, toCompExec } from './output/Abacus.Expr.Token'
import { abacus } from './output/Abacus'

interface ExprEnv {
  vars: {}[]
  funcs: {}[]
  opers: {}[]
}

type JsFunc = (...args: number[]) => number

export const makeCalculator = (funcs: JsFunc[] = []) => {
  return new Calculator({ vars: [], funcs: funcs.map(makeFunc), opers: [] })
}

export class Calculator {
  constructor(private env: ExprEnv) {}

  run(str: string): [string, Calculator] {
    const { env, result } = abacus(this.env)(str)
    return [result, new Calculator(env)]
  }
}

const makeFunc = (func: JsFunc) => {
  const symbol = func.name
  const arity = func.length
  const exec = toCompExec(arity)((args: number[]) => func(...args))
  return Func({ symbol, comp: { arity, exec } })
}
