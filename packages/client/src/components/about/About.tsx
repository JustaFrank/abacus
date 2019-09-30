import React from 'react'
import { RouteComponentProps } from '@reach/router'

import { Page, PageHeading } from '../page/Page'

export const About: React.FC<RouteComponentProps> = () => {
  return (
    <Page>
      <PageHeading>About</PageHeading>
    </Page>
  )
}
