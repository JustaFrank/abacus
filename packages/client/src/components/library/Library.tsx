import React from 'react'
import { RouteComponentProps } from '@reach/router'

import { Page, PageDescription, PageHeading } from '../page/Page'

export const Library: React.FC<RouteComponentProps> = () => {
  return (
    <Page>
      <PageHeading>Library</PageHeading>
      <PageDescription>Functions added to your calculator</PageDescription>
    </Page>
  )
}
