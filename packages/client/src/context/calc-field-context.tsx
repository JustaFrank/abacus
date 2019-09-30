import React, { useReducer } from 'react'

import { useContextSafe } from '../hooks/use-context-safe'

const CalcFieldStateContext = React.createContext<State | null>(null)
const CalcFieldDispatchContext = React.createContext<React.Dispatch<
  Action
> | null>(null)

interface ICalcFieldBase {
  input: string
  result: string
}

interface ICalcField extends ICalcFieldBase {
  index: number
}

type State = ICalcField[]
type Action =
  | { type: 'add'; field: ICalcFieldBase }
  | { type: 'update'; index: number; input: string }

const calcFieldReducer = (fields: State, action: Action) => {
  switch (action.type) {
    case 'add':
      return fields.concat({ ...action.field, index: fields.length })
    case 'update':
      return fields.map(field => {
        return field.index === action.index
          ? { ...field, input: action.input }
          : field
      })
  }
}

export const CalcFieldProvider: React.FC = ({ children }) => {
  const [state, dispatch] = useReducer(calcFieldReducer, [])
  return (
    <CalcFieldStateContext.Provider value={state}>
      <CalcFieldDispatchContext.Provider value={dispatch}>
        {children}
      </CalcFieldDispatchContext.Provider>
    </CalcFieldStateContext.Provider>
  )
}

export const useCalcFieldState = () => {
  return useContextSafe(CalcFieldStateContext)
}

export const useCalcFieldDispatch = () => {
  return useContextSafe(CalcFieldDispatchContext)
}
