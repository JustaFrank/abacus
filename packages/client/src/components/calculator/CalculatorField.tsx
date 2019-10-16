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

  @media (max-width: 768px) {
    display: grid;
    grid-template-rows: 1fr 1fr;
    grid-template-columns: 1em auto;
    grid-template-areas: 'carat input' 'result result';
  }
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

  @media (max-width: 768px) {
    width: calc(100% - 1em);
    grid-area: input;
  }
`

const CalculatorResult = styled.div`
  text-align: right;
  width: 20%;
  padding-right: 24px;
  color: #f39c12;
  font-size: 16px;

  @media (max-width: 768px) {
    width: 100%;
    grid-area: result;
  }
`

export const CalculatorField: React.FC<CalculatorFieldProps> = ({
  children,
  result,
  error
}) => (
  <CalculatorFieldContainer>
    <FaAngleRight size="1em" color="#f39c12" />
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
