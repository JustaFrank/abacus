import React from 'react'
import styled from 'styled-components'
import { FaBars } from 'react-icons/fa'

import { useSidebarContext } from '../../context/sidebar-context'

const SidebarToggleIconContainer = styled.div`
  color: white;
  border-radius: 4px;
  padding: 4px;

  :hover {
    cursor: pointer;
    background-color: #405264;
  }

  :active {
    background-color: #5a6c7c;
  }

  @media (min-width: 769px) {
    display: none;
  }
`

export const SidebarToggle: React.FC = () => {
  const [isOpen, setIsOpen] = useSidebarContext()
  const handleClick = () => setIsOpen(!isOpen)
  return (
    <SidebarToggleIconContainer onClick={handleClick}>
      <FaBars size="1.5em" style={{ display: 'block' }} />
    </SidebarToggleIconContainer>
  )
}
