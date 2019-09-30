import React from 'react'
import { RouteComponentProps } from '@reach/router'

import { Page, PageDescription, PageHeading } from '../page/Page'

export const Marketplace: React.FC<RouteComponentProps> = () => {
  return (
    <Page>
      <PageHeading>Marketplace</PageHeading>
      <PageDescription>Add custom functions to your calculator</PageDescription>
    </Page>
  )
}
