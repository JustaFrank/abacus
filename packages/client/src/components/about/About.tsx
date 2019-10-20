import React from 'react'
import { RouteComponentProps } from '@reach/router'

import { Page, PageDescription, PageHeading } from '../page/Page'

export const About: React.FC<RouteComponentProps> = () => {
  return (
    <Page>
      <PageHeading>About</PageHeading>
      <PageDescription>
        Abacus is a calculator app that allows you to create and use custom
        Javascript functions. Create an account to begin.
        <br />
        <br />
        <a href="https://github.com/franktzheng/abacus">GitHub Repo</a>
      </PageDescription>
    </Page>
  )
}
