import React from 'react'
import styled from 'styled-components'

import { AuthenticatedApp } from './AuthenticatedApp'
import { useUser } from './context/user-context'
import { UnauthenticatedApp } from './UnauthenticatedApp'

const Container = styled.div`
  display: grid;
  height: 100%;
  width: 100%;
  grid-template-rows: auto;
  grid-template-columns: minmax(200px, 1fr) 5fr;
  grid-template-areas: 'sidebar body';

  @media (max-width: 768px) {
    grid-template-rows: max-content auto;
    grid-template-columns: auto;
    grid-template-areas: 'sidebar' 'body';
  }
`

const App: React.FC = () => {
  const { user } = useUser()
  return (
    <Container>
      {user ? <AuthenticatedApp /> : <UnauthenticatedApp />}
    </Container>
  )
}

export default App
