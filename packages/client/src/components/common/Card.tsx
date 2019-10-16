import React from 'react'
import styled from 'styled-components'

import { Button } from '../common/Button'

interface CardProps {
  title: string
  buttonText?: string
  onClick?: () => any
}

const CardContainer = styled.div`
  padding: 16px;
  border: 1px solid #cbcbcb;
  border-radius: 8px;
`

const CardTitle = styled.div`
  font-weight: bolder;
  font-size: 18px;
  margin-bottom: 12px;
`

const CardDescription = styled.div`
  height: 48px;
  margin-bottom: 12px;
`

export const Card: React.FC<CardProps> = ({
  title,
  buttonText,
  onClick,
  children
}) => {
  return (
    <CardContainer>
      <CardTitle>{title}</CardTitle>
      <CardDescription>{children}</CardDescription>
      <div style={{ marginTop: 'auto' }}>
        {buttonText && (
          <Button backgroundColor="orange" onClick={onClick}>
            {buttonText}
          </Button>
        )}
      </div>
    </CardContainer>
  )
}
