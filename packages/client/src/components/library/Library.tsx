import React from 'react'
import { RouteComponentProps } from '@reach/router'
import styled from 'styled-components'

import { Page, PageDescription, PageHeading } from '../page/Page'
import { useUser } from '../../context/user-context'
import { Card } from '../common/Card'

const LibraryGrid = styled.div`
  display: grid;
  height: 100%;
  width: 100%;
  grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
  grid-gap: 12px;
`

export const Library: React.FC<RouteComponentProps> = () => {
  const { user } = useUser()
  const functions = user ? user.addedFunctions : []
  return (
    <Page>
      <PageHeading>Library</PageHeading>
      <PageDescription>Functions added to your calculator</PageDescription>
      <LibraryGrid>
        {functions.map(({ id, name, description }) => (
          <Card key={id} title={name} buttonText="remove" onClick={() => {}}>
            {description}
          </Card>
        ))}
      </LibraryGrid>
    </Page>
  )
}
