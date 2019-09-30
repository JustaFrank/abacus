import React from 'react'
import { RouteComponentProps } from '@reach/router'

import { Page, PageDescription, PageHeading } from '../page/Page'

export const Calculator: React.FC<RouteComponentProps> = () => {
  return (
    <Page>
      <PageHeading>Calculator</PageHeading>
      <PageDescription>Type an expression and press ENTER</PageDescription>
    </Page>
  )
}
