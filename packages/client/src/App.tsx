import React from 'react'
import styled from 'styled-components'

import * as Grid from './components/grid/Grid'
import logo from './assets/logo.svg'

const Red = styled.div`
  background-color: #2c3e50;
  height: 100%;
  width: 100%;
`

const App: React.FC = () => {
  return (
    <>
      <Grid.Container
        rows="[start] auto [end]"
        columns="[start] 1fr [middle] 4fr [end]"
      >
        <Grid.Item rows={['start', 'end']} columns={['start', 'middle']}>
          <Red>
            <img src={logo}></img>
          </Red>
        </Grid.Item>
      </Grid.Container>
    </>
  )
}

export default App
