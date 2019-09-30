import React, { useState } from 'react'
import styled from 'styled-components'
import { Link } from '@reach/router'
import { FaBars } from 'react-icons/fa'

import logo from '../../../assets/logo.svg'
import { Flex } from '../../utils/Flex'

interface SidebarToggleProps {
  isOpen: boolean
  onClick: (isOpen: boolean) => void
}

const SidebarBackground = styled.div`
  background-color: #273746;
  height: 100%;
  width: 100%;
`

const SidebarCollapse = styled.div<{ isOpen: boolean }>`
  @media (max-width: 768px) {
    padding-bottom: 8px;
    display: ${({ isOpen }) => (isOpen ? 'block' : 'none')};
    position: absolute;
    width: 100%;
  }
`

const SidebarHeaderContainer = styled(Flex)`
  padding: 18px;

  @media (max-width: 768px) {
    padding: 12px 18px;
  }
`

const SidebarToggleContainer = styled.div`
  float: left;
  position: absolute;
`

export const Sidebar: React.FC = ({ children }) => {
  const [isOpen, setIsOpen] = useState(false)
  return (
    <SidebarBackground>
      <SidebarHeaderContainer alignItems="center">
        <SidebarToggleContainer>
          <SidebarToggle isOpen={isOpen} onClick={setIsOpen} />
        </SidebarToggleContainer>
        <Flex justifyContent="center" fullWidth>
          <SidebarLogo />
        </Flex>
      </SidebarHeaderContainer>
      <SidebarCollapse isOpen={isOpen}>{children}</SidebarCollapse>
    </SidebarBackground>
  )
}

const SidebarLogo: React.FC = () => {
  return (
    <Link to="/">
      <img src={logo} alt="abacus" width="120px"></img>
    </Link>
  )
}

const SidebarToggleIconContainer = styled.div`
  color: white;
  border-radius: 4px;
  padding: 4px;

  :hover {
    cursor: pointer;
    background-color: #5a6c7c;
  }

  @media (min-width: 769px) {
    display: none;
  }
`

const SidebarToggle: React.FC<SidebarToggleProps> = ({ onClick, isOpen }) => {
  return (
    <SidebarToggleIconContainer onClick={() => onClick(!isOpen)}>
      <FaBars size="1.5em" style={{ display: 'block' }} />
    </SidebarToggleIconContainer>
  )
}
