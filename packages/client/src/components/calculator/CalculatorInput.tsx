import React from 'react'
import styled from 'styled-components'

import { CalculatorField } from './CalculatorField'

interface CalculatorFieldProps {
  input: string
  result: string
  handleChange: (value: string) => any
  handleEnter: () => any
  inputRef?: React.MutableRefObject<HTMLInputElement | null>
}

const UnstyledInput = styled.input`
  width: 100%;
  border: none;
  outline: none;
  font: inherit;
  padding: 0;
  height: inherit;
`

export const CalculatorInput: React.FC<CalculatorFieldProps> = ({
  input,
  result,
  handleChange,
  handleEnter,
  inputRef
}) => {
  const handleKeyPress = (event: React.KeyboardEvent) => {
    if (event.charCode === 13) {
      handleEnter()
    }
  }

  return (
    <CalculatorField result={result}>
      <UnstyledInput
        ref={inputRef}
        type="text"
        value={input}
        onChange={event => handleChange(event.target.value)}
        onKeyPress={handleKeyPress}
      />
    </CalculatorField>
  )
}
