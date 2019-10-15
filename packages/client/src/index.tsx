import React from 'react'
import ReactDOM from 'react-dom'
import { ApolloProvider } from '@apollo/react-hooks'
import ApolloClient from 'apollo-boost'

import App from './App'
import { AppProviders } from './AppProviders'
import * as serviceWorker from './service-worker'

import './css/index.css'
import './css/reset.css'

const client = new ApolloClient({
  uri: 'http://localhost:8080'
})

ReactDOM.render(
  <ApolloProvider client={client}>
    <AppProviders>
      <App />
    </AppProviders>
  </ApolloProvider>,
  document.getElementById('root')
)

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister()
