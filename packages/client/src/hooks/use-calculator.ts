import { useRef, useState } from 'react'
import { Calculator, makeCalculator } from '@abacus/calculator'

interface Field {
  input: string
  result: string
}

export const useCalculator = () => {
  const initialField = { input: '', result: '' }

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
    setActiveField({
      ...activeField,
      input,
      ...(!isNaN(parseFloat(result)) && { result })
    })
  }

  return { activeField, inactiveFields, handleChange, next }
}
