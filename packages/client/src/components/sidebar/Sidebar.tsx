import React, { useState, useRef } from 'react'
import { Link } from '@reach/router'
import styled from 'styled-components'

import { SidebarToggle } from './SidebarToggle'
import { Flex } from '../utils/Flex'

import logo from '../../assets/logo.svg'
import { useMousedownOutside } from '../../hooks/use-mousedown'
import {
  SidebarContextProvider,
  useSidebarContext
} from '../../context/sidebar-context'

interface SidebarCollapseProps {
  isOpen: boolean
}

const SidebarBackground = styled.div`
  background-color: #273746;
  height: 100%;
  width: 100%;
`

const SidebarCollapse = styled.div<SidebarCollapseProps>`
  background-color: #273746;

  @media (max-width: 768px) {
    padding-bottom: 8px;
    display: ${({ isOpen }) => (isOpen ? 'block' : 'none')};
    width: 100%;
    position: absolute;
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
  const [isOpen, setIsOpen] = useSidebarContext()
  const sidebarElem = useRef<HTMLDivElement>(null)
  useMousedownOutside(sidebarElem, () => setIsOpen(false))
  return (
    <SidebarBackground ref={sidebarElem}>
      <SidebarHeaderContainer alignItems="center">
        <SidebarToggleContainer>
          <SidebarToggle />
        </SidebarToggleContainer>
        <Flex justifyContent="center" fullWidth>
          <SidebarLogo />
        </Flex>
      </SidebarHeaderContainer>
      <SidebarCollapse isOpen={isOpen}>{children}</SidebarCollapse>
    </SidebarBackground>
  )
}

export const SidebarLogo: React.FC = () => {
  return (
    <Link to="/">
      <img src={logo} alt="abacus" width="120px" />
    </Link>
  )
}
