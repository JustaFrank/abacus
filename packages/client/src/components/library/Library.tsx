import React from 'react'
import { RouteComponentProps } from '@reach/router'
import styled from 'styled-components'

import { Page, PageDescription, PageHeading } from '../page/Page'
import { LibraryProvider, useLibrary } from '../../context/library-context'
import { Card } from '../common/Card'

const LibraryGrid = styled.div`
  display: grid;
  height: 100%;
  width: 100%;
  grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
  grid-gap: 12px;
`

const LibraryPage: React.FC = () => {
  const { functions, removeFunction } = useLibrary()
  return (
    <Page>
      <PageHeading>Library</PageHeading>
      <PageDescription>Functions added to your calculator</PageDescription>
      <LibraryGrid>
        {functions.map(({ id, name, description }) => (
          <Card
            key={id}
            title={name}
            buttonText="remove"
            onClick={() => removeFunction(id)}
          >
            {description}
          </Card>
        ))}
      </LibraryGrid>
    </Page>
  )
}

export const Library: React.FC<RouteComponentProps> = () => {
  return (
    <LibraryProvider>
      <LibraryPage />
    </LibraryProvider>
  )
}
