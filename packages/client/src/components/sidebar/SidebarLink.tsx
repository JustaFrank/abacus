import React from 'react'
import styled from 'styled-components'
import { Link } from '@reach/router'

interface SidebarLinkProps {
  to: string
}

const SidebarLinkAnchor = styled(Link)`
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

export const SidebarLink: React.FC<SidebarLinkProps> = ({ to, children }) => {
  return (
    <SidebarLinkAnchor
      to={to}
      getProps={({ isCurrent }) =>
        isCurrent ? { style: { backgroundColor: '#5a6c7c' } } : {}
      }
    >
      <SidebarLinkText>{children}</SidebarLinkText>
    </SidebarLinkAnchor>
  )
}
