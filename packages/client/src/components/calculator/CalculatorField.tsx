import React, { useState, useRef, useEffect } from 'react'
import { FaAngleRight } from 'react-icons/fa'
import styled from 'styled-components'

interface CalculatorFieldProps {
  input: string
  result: string
  handleChange: (value: string) => any
  handleEnter: () => any
  disabled?: boolean
}

const CalculatorFieldContainer = styled.div`
  display: flex;
  align-items: center;
`

const CalculatorInput = styled.input`
  padding: 6px 0;
  margin: 0 12px;
  border: none;
  width: 80%;
  outline: none;
  font-family: inherit;
  font-size: 16px;
  font-weight: lighter;

  :disabled {
    background-color: inherit;
  }
`

const CalculatorResult = styled.div`
  text-align: right;
  width: 20%;
  padding-right: 24px;
  color: #f39c12;
  font-size: 16px;
`

export const CalculatorField: React.FC<CalculatorFieldProps> = ({
  input,
  result,
  handleChange,
  handleEnter,
  disabled = false
}) => {
  const inputRef = useRef<HTMLInputElement | null>(null)
  // const result =

  // const [result, setResult] = useState<string>('')
  // const [input, setInput] = useState<string>('')

  // const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
  //   setInput(event.target.value)
  //   onChange(event.target.value)
  //   const calcResult = calculator.run(event.target.value)
  //   if (!isNaN(Number(calcResult))) {
  //     setResult(calculator.run(event.target.value))
  //   } else {
  //     setResult('')
  //   }
  // }

  const handleKeyPress = (event: React.KeyboardEvent) => {
    if (event.charCode === 13) {
      handleEnter()
    }
  }

  useEffect(() => {
    if (!disabled && inputRef.current) {
      inputRef.current.focus()
    }
  }, [])

  return (
    <CalculatorFieldContainer>
      <FaAngleRight color="#f39c12" />
      <CalculatorInput
        ref={inputRef}
        value={input}
        onChange={event => handleChange(event.target.value)}
        onKeyPress={handleKeyPress}
        disabled={disabled}
        spellCheck={false}
      />
      <CalculatorResult>{result}</CalculatorResult>
    </CalculatorFieldContainer>
  )
}
