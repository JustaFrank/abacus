import { useRef, useState } from 'react'
import { Calculator, makeCalculator } from '@abacus/calculator'

interface Field {
  input: string
  result: string
  error: boolean
}

export const useCalculator = () => {
  const initialField = { input: '', result: '', error: false }

  const prevCalculator = useRef<Calculator | null>(null)

  const [calculator, setCalculator] = useState(makeCalculator())
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
