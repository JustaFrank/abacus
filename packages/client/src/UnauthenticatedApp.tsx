import React from 'react'
import { Router, Location, navigate } from '@reach/router'
import styled from 'styled-components'

import { About } from './components/about/About'
import { Calculator } from './components/calculator/Calculator'
import { Login } from './components/login/Login'
import { Sidebar } from './components/sidebar/Sidebar'
import { SidebarButton } from './components/sidebar/SidebarButton'
import { SidebarLink } from './components/sidebar/SidebarLink'
import { Register } from './components/login/Register'

const SidebarContainer = styled.div`
  grid-area: sidebar;
`

const BodyContainer = styled.div`
  grid-area: body;
  overflow-y: auto;
`

export const UnauthenticatedApp: React.FC = () => {
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
  return (
    <SidebarContainer>
      <Sidebar>
        <SidebarLink to="/">calculator</SidebarLink>
        <SidebarLink to="/about">about</SidebarLink>
        <Location>
          {({ location }) =>
            location.pathname !== '/login' && (
              <SidebarButton onClick={() => navigate('/login')}>
                login
              </SidebarButton>
            )
          }
        </Location>
      </Sidebar>
    </SidebarContainer>
  )
}

const AppRouter: React.FC = () => {
  return (
    <Router>
      <Calculator path="/" />
      <Register path="/register" />
      <Login path="/login" />
      <About path="/about" />
    </Router>
  )
}
