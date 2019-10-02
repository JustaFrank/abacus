import React from 'react'
import { Router } from '@reach/router'
import styled from 'styled-components'

import { About } from './components/about/About'
import { Calculator } from './components/calculator/Calculator'
import { Create } from './components/create/Create'
import { Library } from './components/library/Library'
import { Marketplace } from './components/marketplace/Marketplace'
import { Sidebar } from './components/sidebar/Sidebar'
import { SidebarContextProvider } from './context/sidebar-context'
import { SidebarLink } from './components/sidebar/SidebarLink'

const Container = styled.div`
  display: grid;
  height: 100%;
  width: 100%;
  grid-template-rows: auto;
  grid-template-columns: minmax(200px, 1fr) 5fr;
  grid-template-areas: 'sidebar body';

  @media (max-width: 768px) {
    grid-template-rows: max-content auto;
    grid-template-columns: auto;
    grid-template-areas: 'sidebar' 'body';
  }
`

const SidebarContainer = styled.div`
  grid-area: sidebar;
`

const BodyContainer = styled.div`
  grid-area: body;
  overflow-y: auto;
`

const App: React.FC = () => {
  return (
    <>
      <SidebarContextProvider>
        <Container>
          <SidebarContainer>
            <Sidebar>
              <SidebarLink to="/calculator">calculator</SidebarLink>
              <SidebarLink to="/library">library</SidebarLink>
              <SidebarLink to="/marketplace">marketplace</SidebarLink>
              <SidebarLink to="/create">create</SidebarLink>
              <SidebarLink to="/about">about</SidebarLink>
            </Sidebar>
          </SidebarContainer>
          <BodyContainer>
            <Router>
              <Calculator path="/calculator" />
              <Library path="/library" />
              <Marketplace path="/marketplace" />
              <Create path="/create" />
              <About path="/about" />
            </Router>
          </BodyContainer>
        </Container>
      </SidebarContextProvider>
    </>
  )
}

export default App
