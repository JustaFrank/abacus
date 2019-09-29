import React from 'react'

import { GridContainer, GridItem } from './components/grid/Grid'
import { Sidebar, SidebarLink, SidebarLogo } from './components/sidebar/Sidebar'

const App: React.FC = () => {
  return (
    <>
      <GridContainer
        rows="[start] auto [end]"
        columns="[start] minmax(180px, 1fr) [middle] 5fr [end]"
      >
        <GridItem
          rowStart="start"
          rowEnd="end"
          columnStart="start"
          columnEnd="middle"
        >
          <Sidebar>
            <SidebarLogo />
            <SidebarLink to="/calculator">calculator</SidebarLink>
            <SidebarLink to="/library">library</SidebarLink>
            <SidebarLink to="/marketplace">marketplace</SidebarLink>
            <SidebarLink to="/create">create</SidebarLink>
            <SidebarLink to="/about">about</SidebarLink>
          </Sidebar>
        </GridItem>
      </GridContainer>
    </>
  )
}

export default App
