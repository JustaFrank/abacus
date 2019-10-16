import React from 'react'
import styled from 'styled-components'

import { Button } from '../common/Button'
import { useSidebarContext } from '../../context/sidebar-context'

const SidebarButtonContainer = styled.div`
  padding-top: 12px;
  display: flex;
  justify-content: center;
`

export const SidebarButton: React.FC<
  React.HTMLProps<HTMLButtonElement>
> = props => {
  const [, setIsOpen] = useSidebarContext()
  return (
    <SidebarButtonContainer onClick={() => setIsOpen(false)}>
      <Button {...props} width="80%" />
    </SidebarButtonContainer>
  )
}
