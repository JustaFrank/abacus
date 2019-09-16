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

export default class Calculator {
  env: ExprEnv

  constructor(funcs: JsFunc[] = []) {
    this.env = { vars: [], funcs: funcs.map(makeFunc), opers: [] }
  }

  run(str: string): string {
    const { env, result } = abacus(this.env)(str)
    this.env = env
    return result
  }
}

function makeFunc(func: JsFunc) {
  const symbol = func.name
  const arity = func.length
  const exec = toCompExec(arity)((args: number[]) => func(...args))
  return Func({ symbol, comp: { arity, exec } })
}
