import styled from 'styled-components'

type Alignment = 'start' | 'end' | 'center' | 'stretch'

interface GridContainerProps {
  columns: string
  rows: string
  rowGap?: string
  columnGap?: string
  justifyItems?: Alignment
  alignItems?: Alignment
  justifyContent?: Alignment
  alignContent?: Alignment
}

interface GridItemProps {
  rowStart: string
  rowEnd: string
  columnStart: string
  columnEnd: string
}

export const GridContainer = styled.div<GridContainerProps>`
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

export const GridItem = styled.div<GridItemProps>`
  grid-row-start: ${props => props.rowStart};
  grid-row-end: ${props => props.rowEnd};
  grid-column-start: ${props => props.columnStart};
  grid-column-end: ${props => props.columnEnd};
`
