import React from 'react'
import { RouteComponentProps } from '@reach/router'

import { Page, PageDescription, PageHeading } from '../page/Page'

export const Create: React.FC<RouteComponentProps> = () => {
  return (
    <Page>
      <PageHeading>Create</PageHeading>
      <PageDescription>Create a custom Javascript function</PageDescription>
    </Page>
  )
}
