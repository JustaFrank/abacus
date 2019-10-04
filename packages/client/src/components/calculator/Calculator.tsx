import React, { useRef } from 'react'
import styled from 'styled-components'
import { RouteComponentProps } from '@reach/router'

import { CalculatorField } from './CalculatorField'
import { CalculatorInput } from './CalculatorInput'
import { useCalculator } from '../../hooks/use-calculator'
import { Page, PageDescription, PageHeading } from '../page/Page'

const CalculatorFieldsContainer = styled.div`
  height: calc(100vh - 136px);
  cursor: text;
  overflow-y: auto;
`

export const Calculator: React.FC<RouteComponentProps> = () => {
  const activeInputRef = useRef<HTMLInputElement | null>(null)
  const { inactiveFields, activeField, next, handleChange } = useCalculator()

  const handleClick = () => {
    setTimeout(() => {
      const selection = window.getSelection()
      if (activeInputRef.current && selection && selection.toString() === '') {
        activeInputRef.current.focus()
      }
    })
  }

  return (
    <Page>
      <PageHeading>Calculator</PageHeading>
      <PageDescription>Type any expression and press ENTER.</PageDescription>
      <CalculatorFieldsContainer onClick={handleClick}>
        {inactiveFields.map(({ input, result }, idx) => (
          <CalculatorField result={result} key={idx}>
            {input}
          </CalculatorField>
        ))}
        <CalculatorInput
          {...activeField}
          handleChange={handleChange}
          handleEnter={next}
          inputRef={activeInputRef}
        />
      </CalculatorFieldsContainer>
    </Page>
  )
}
