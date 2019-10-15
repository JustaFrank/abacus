import React from 'react'
import { FirebaseProvider } from './context/firebase-context'
import { UserProvider } from './context/user-context'
import { SidebarProvider } from './context/sidebar-context'

export const AppProviders: React.FC = ({ children }) => {
  return (
    <FirebaseProvider>
      <UserProvider>
        <SidebarProvider>{children}</SidebarProvider>
      </UserProvider>
    </FirebaseProvider>
  )
}
