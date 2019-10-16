import { useRef, useState } from 'react'
import { Calculator, makeCalculator } from '@abacus/calculator'
import { evaluate } from '@abacus/eval'
import { useUser } from '../context/user-context'

interface Field {
  input: string
  result: string
  error: boolean
}

export const useCalculator = () => {
  const initialField = { input: '', result: '', error: false }

  const prevCalculator = useRef<Calculator | null>(null)

  const { user } = useUser()
  const funcs = user
    ? user.addedFunctions.map(({ body }) => {
        console.log(evaluate(body)(3, 4))
        return evaluate(body)
      })
    : []

  const [calculator, setCalculator] = useState(makeCalculator(funcs))
  const [activeField, setActiveField] = useState<Field>(initialField)
  const [inactiveFields, setInactiveFields] = useState<Field[]>([] as any[])

  const next = () => {
    setInactiveFields(inactiveFields.concat(activeField))
    setActiveField(initialField)
    prevCalculator.current && setCalculator(prevCalculator.current)
  }

  const handleChange = (input: string) => {
    const [result, newCalculator] = calculator.run(input)
    prevCalculator.current = newCalculator
    if (input.trim() === '') {
      setActiveField({
        ...activeField,
        input,
        result: '',
        error: false
      })
    } else if (isNaN(parseFloat(result))) {
      setActiveField({
        ...activeField,
        input,
        error: true
      })
    } else {
      setActiveField({
        ...activeField,
        input,
        result,
        error: false
      })
    }
  }

  return { activeField, inactiveFields, handleChange, next }
}
