import React from 'react'
import { Router } from '@reach/router'
import styled from 'styled-components'

import { About } from './components/about/About'
import { Calculator } from './components/calculator/Calculator'
import { Create } from './components/create/Create'
import { Library } from './components/library/Library'
import { Login } from './components/login/Login'
import { Marketplace } from './components/marketplace/Marketplace'
import { Sidebar } from './components/sidebar/Sidebar'
import { SidebarButton } from './components/sidebar/SidebarButton'
import { SidebarLink } from './components/sidebar/SidebarLink'
import { useUser } from './context/user-context'

const SidebarContainer = styled.div`
  grid-area: sidebar;
`

const BodyContainer = styled.div`
  grid-area: body;
  overflow-y: auto;
`

export const AuthenticatedApp: React.FC = () => {
  return (
    <>
      <AppSidebar />
      <BodyContainer>
        <AppRouter />
      </BodyContainer>
    </>
  )
}

const AppSidebar: React.FC = () => {
  const { logout } = useUser()
  return (
    <SidebarContainer>
      <Sidebar>
        <SidebarLink to="/">calculator</SidebarLink>
        <SidebarLink to="/library">library</SidebarLink>
        <SidebarLink to="/marketplace">marketplace</SidebarLink>
        <SidebarLink to="/create">create</SidebarLink>
        <SidebarLink to="/about">about</SidebarLink>
        <SidebarButton onClick={logout}>logout</SidebarButton>
      </Sidebar>
    </SidebarContainer>
  )
}

const AppRouter: React.FC = () => {
  return (
    <Router>
      <Calculator path="/" />
      <Library path="/library" />
      <Marketplace path="/marketplace" />
      <Create path="/create" />
      <About path="/about" />
    </Router>
  )
}
