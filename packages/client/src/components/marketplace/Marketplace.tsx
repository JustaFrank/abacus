import React from 'react'
import { RouteComponentProps } from '@reach/router'
import styled from 'styled-components'

import { Card } from '../common/Card'
import {
  MarketplaceProvider,
  useMarketplace
} from '../../context/marketplace-context'
import { Page, PageDescription, PageHeading } from '../page/Page'

const MarketplaceGrid = styled.div`
  display: grid;
  height: 100%;
  width: 100%;
  grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
  grid-gap: 12px;
`

const MarketplacePage: React.FC = () => {
  const { functions, addFunction } = useMarketplace()
  return (
    <Page>
      <PageHeading>Marketplace</PageHeading>
      <PageDescription>
        Discover and add new functions to your library.
      </PageDescription>
      <MarketplaceGrid>
        {functions.map(({ id, name, description }) => (
          <Card
            key={id}
            title={name}
            buttonText="add to library"
            onClick={() => addFunction(id)}
          >
            {description}
          </Card>
        ))}
      </MarketplaceGrid>
    </Page>
  )
}

export const Marketplace: React.FC<RouteComponentProps> = () => {
  return (
    <MarketplaceProvider>
      <MarketplacePage />
    </MarketplaceProvider>
  )
}
