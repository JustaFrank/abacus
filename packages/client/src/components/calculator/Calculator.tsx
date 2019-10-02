import React, { useState, useRef } from 'react'
import styled from 'styled-components'
import { RouteComponentProps } from '@reach/router'

import { Page, PageDescription, PageHeading } from '../page/Page'
import { CalculatorField } from './CalculatorField'

import {
  Calculator as CalculatorType,
  makeCalculator
} from '@abacus/calculator'

interface ICalculatorField {
  input: string
  result: string
  disabled: boolean
}

const CalculatorFieldsContainer = styled.div`
  height: calc(100vh - 136px);
  overflow-y: auto;
`

export const Calculator: React.FC<RouteComponentProps> = () => {
  const { fields, add, compute } = useCalculator()
  return (
    <Page>
      <PageHeading>Calculator</PageHeading>
      <PageDescription>Type any expression to begin</PageDescription>
      <CalculatorFieldsContainer>
        {fields.map((props, idx) => (
          <CalculatorField
            {...props}
            key={idx}
            handleEnter={add}
            handleChange={compute}
          />
        ))}
      </CalculatorFieldsContainer>
    </Page>
  )
}

const useCalculator = () => {
  const prevCalculator = useRef<CalculatorType | null>(null)

  const [calculator, setCalculator] = useState(makeCalculator())
  const [activeField, setActiveField] = useState<ICalculatorField>({
    input: '',
    result: '',
    disabled: false
  })
  const [oldFields, setOldFields] = useState<ICalculatorField[]>(
    [] as ICalculatorField[]
  )

  const add = () => {
    setOldFields(oldFields.concat({ ...activeField, disabled: true }))
    setActiveField({ input: '', result: '', disabled: false })
    prevCalculator.current && setCalculator(prevCalculator.current)
  }

  const compute = (input: string) => {
    const [result, newCalculator] = calculator.run(input)
    prevCalculator.current = newCalculator
    if (!isNaN(parseFloat(result))) {
      setActiveField({ ...activeField, input, result })
    } else {
      setActiveField({ ...activeField, input })
    }
  }

  return { add, compute, fields: oldFields.concat(activeField) }
}
