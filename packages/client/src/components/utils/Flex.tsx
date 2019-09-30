import React from 'react'
import styled from 'styled-components'

type ContentAlignment =
  | 'stretch'
  | 'center'
  | 'flex-start'
  | 'flex-end'
  | 'space-between'
  | 'space-around'
  | 'initial'
  | 'inherit'

type ItemAlignment =
  | 'stretch'
  | 'center'
  | 'flex-start'
  | 'flex-end'
  | 'baseline'
  | 'initial'
  | 'inherit'

interface FlexProps {
  alignContent?: ContentAlignment
  justifyContent?: ContentAlignment
  alignItems?: ItemAlignment
  justifyItems?: ItemAlignment
  fullHeight?: boolean
  fullWidth?: boolean
}

interface FlexContainerProps {
  fullHeight?: boolean
  fullWidth?: boolean
}

const FlexContainer = styled.div<FlexContainerProps>`
  display: flex;
  box-sizing: border-box;
  height: ${({ fullHeight }) => (fullHeight ? '100%' : 'auto')};
  width: ${({ fullWidth }) => (fullWidth ? '100%' : 'auto')};
`

export const Flex: React.FC<FlexProps> = ({
  alignContent,
  justifyContent,
  alignItems,
  justifyItems,
  ...props
}) => {
  return (
    <FlexContainer
      {...props}
      style={{ alignContent, justifyContent, alignItems, justifyItems }}
    />
  )
}
