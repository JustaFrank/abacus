import React from 'react'
import { FaAngleRight, FaExclamationTriangle } from 'react-icons/fa'
import styled from 'styled-components'

interface CalculatorFieldProps {
  result?: string
  error?: boolean
}

const CalculatorFieldContainer = styled.div`
  display: flex;
  align-items: center;
`

const CalculatorInputContainer = styled.div`
  padding: 6px 0;
  margin: 0 12px;
  width: 80%;
  font-size: 16px;
  font-weight: lighter;
  display: flex;
  align-items: center;
  height: 18px;
`

const CalculatorResult = styled.div`
  text-align: right;
  width: 20%;
  padding-right: 24px;
  color: #f39c12;
  font-size: 16px;
`

export const CalculatorField: React.FC<CalculatorFieldProps> = ({
  children,
  result,
  error
}) => (
  <CalculatorFieldContainer>
    <FaAngleRight color="#f39c12" />
    <CalculatorInputContainer>{children}</CalculatorInputContainer>
    {error ? (
      <CalculatorResult>
        <FaExclamationTriangle color="red" />
      </CalculatorResult>
    ) : result ? (
      <CalculatorResult>{result}</CalculatorResult>
    ) : null}
  </CalculatorFieldContainer>
)
