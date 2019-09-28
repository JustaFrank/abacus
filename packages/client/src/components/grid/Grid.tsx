import styled from 'styled-components'

type Alignment = 'start' | 'end' | 'center' | 'stretch'

interface ContainerProps {
  columns: string
  rows: string
  rowGap?: string
  columnGap?: string
  justifyItems?: Alignment
  alignItems?: Alignment
  justifyContent?: Alignment
  alignContent?: Alignment
}

interface ItemProps {
  columns: [string, string]
  rows: [string, string]
}

export const Container = styled.div<ContainerProps>`
  display: grid;
  height: 100%;
  width: 100%;
  grid-template-rows: ${props => props.rows};
  grid-template-columns: ${props => props.columns};
  grid-row-gap: ${props => props.rowGap};
  grid-column-gap: ${props => props.columnGap};
  align-items: ${props => props.alignItems};
  justify-items: ${props => props.justifyItems};
  align-content: ${props => props.alignContent};
  justify-content: ${props => props.justifyContent};
`

export const Item = styled.div<ItemProps>`
  grid-row-start: ${props => props.rows[0]};
  grid-row-end: ${props => props.rows[1]};
  grid-column-start: ${props => props.columns[0]};
  grid-column-end: ${props => props.columns[1]};
`
