import React from 'react'
import { BeatLoader } from 'react-spinners'
import styled from 'styled-components'

interface FullPageSpinnerProps {
  color?: string
  backgroundColor?: string
}

const SpinnerContainer = styled.div<{ backgroundColor: string }>`
  background-color: ${({ backgroundColor }) => backgroundColor};
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100vh;
`

export const FullPageSpinner: React.FC<FullPageSpinnerProps> = ({
  color = 'white',
  backgroundColor = '#273746'
}) => {
  return (
    <SpinnerContainer backgroundColor={backgroundColor}>
      <BeatLoader color={color} />
    </SpinnerContainer>
  )
}
