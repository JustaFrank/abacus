import React from 'react'
import styled from 'styled-components'
import { Link } from '@reach/router'

import logo from '../../assets/logo.svg'

interface SidebarLinkProps {
  to: string
}

const SidebarBackground = styled.div`
  background-color: #273746;
  height: 100%;
  width: 100%;
`

export const Sidebar: React.FC = ({ children }) => {
  return <SidebarBackground>{children}</SidebarBackground>
}

const SidebarLogoContainer = styled.div`
  display: flex;
  justify-content: center;
  padding: 24px 12px 18px 0;
`

export const SidebarLogo: React.FC = () => {
  return (
    <SidebarLogoContainer>
      <Link to="/">
        <img src={logo} alt="abacus" width="120px"></img>
      </Link>
    </SidebarLogoContainer>
  )
}

const SidebarLinkBackground = styled(Link)`
  display: flex;
  justify-content: center;
  width: 100%;
  padding: 12px 0 12px 0;
  color: white;

  :hover {
    background-color: #405264;
    cursor: pointer;
  }

  :active {
    background-color: #5a6c7c;
  }
`

const SidebarLinkText = styled.div`
  color: white;
  font-size: 14px;
  font-weight: bolder;
`

export const SidebarLink: React.FC<SidebarLinkProps> = ({ children, to }) => {
  return (
    <SidebarLinkBackground
      to={to}
      getProps={({ isCurrent }) =>
        isCurrent ? { style: { backgroundColor: '#5a6c7c' } } : {}
      }
    >
      <SidebarLinkText>{children}</SidebarLinkText>
    </SidebarLinkBackground>
  )
}
