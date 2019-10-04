import React, { useState } from 'react'
import { useContextSafe } from '../hooks/use-context-safe'

type SidebarContextValue = [boolean, (value: boolean) => any]

const SidebarContext = React.createContext<SidebarContextValue | null>(null)

export const SidebarContextProvider: React.FC = props => {
  const [isOpen, setIsOpen] = useState(false)
  const toggle = (value: boolean) => setIsOpen(() => value)
  return <SidebarContext.Provider {...props} value={[isOpen, toggle]} />
}

export const useSidebarContext = () => {
  return useContextSafe(SidebarContext, 'Sidebar')
}
