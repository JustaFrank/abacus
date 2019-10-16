import React from 'react'
import styled from 'styled-components'
import { BeatLoader } from 'react-spinners'

interface TextButtonProps {
  width?: string
  backgroundColor?: keyof typeof colorSets
  color?: string
}

interface ButtonProps extends TextButtonProps {
  loading?: boolean
}

const colorSets = {
  gray: {
    base: '#405264',
    hover: '#5a6c7c',
    active: '#6b7d8e;'
  },
  orange: {
    base: '#fad7a0',
    hover: '#f5be6a',
    active: '#f3b350'
  }
}

const getColor = (
  type: 'base' | 'hover' | 'active',
  color?: keyof typeof colorSets
) => {
  return color ? colorSets[color][type] : colorSets.gray[type]
}

const TextButton = styled.button<ButtonProps>`
  border: none;
  cursor: pointer;
  outline: none;
  font-family: inherit;

  font-size: 14px;
  font-weight: bolder;
  border-radius: 8px;
  padding: 10px 0 10px 0;
  color: ${({ color }) => color || 'white'};
  width: ${({ width }) => width || '100%'};
  background-color: ${({ backgroundColor }) =>
    getColor('base', backgroundColor)};

  :hover {
    background-color: ${({ backgroundColor }) =>
      getColor('hover', backgroundColor)};
  }

  :active {
    background-color: ${({ backgroundColor }) =>
      getColor('active', backgroundColor)};
  }
`

// Any casts because I can't find a workaround
export const Button: React.FC<
  ButtonProps & React.HTMLProps<HTMLButtonElement>
> = ({ loading, ...props }) => {
  return loading ? (
    <TextButton
      {...(props as any)}
      children={<BeatLoader size={8} color="white" />}
    />
  ) : (
    <TextButton {...(props as any)} />
  )
}
