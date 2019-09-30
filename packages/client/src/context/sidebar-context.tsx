import React, { useState, useContext } from 'react'
import { useContextSafe } from '../utils/context'

type SidebarContextValue = [boolean, (value: boolean) => any]

const SidebarContext = React.createContext<SidebarContextValue | null>(null)

export const SidebarContextProvider: React.FC = props => {
  const [isOpen, setIsOpen] = useState(false)
  const toggle = (value: boolean) => setIsOpen(() => value)
  return <SidebarContext.Provider {...props} value={[isOpen, toggle]} />
}

export const useSidebarContext = () => {
  const value = useContext(SidebarContext)
  if (value === null) {
    throw new Error('Missing context provider')
  }
  return value
}
