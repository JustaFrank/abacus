import React from 'react'
import styled from 'styled-components'

import { Sidebar } from './components/common/sidebar/Sidebar'
import { SidebarLink } from './components/common/sidebar/SidebarLink'

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

const App: React.FC = () => {
  return (
    <>
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
      </Container>
    </>
  )
}

export default App
