import React, { useState, useEffect, useCallback } from 'react'
import { useMutation } from '@apollo/react-hooks'
import gql from 'graphql-tag'
import { navigate } from '@reach/router'
import { useAsync } from 'react-async'

import { useContextSafe } from '../hooks/use-context-safe'
import { useFirebase } from './firebase-context'

interface UserContextValue {
  isAuthenticated: boolean
  login: (email: string, password: string) => void
  logout: () => void
  register: (name: string, email: string, password: string) => void
}

const CREATE_USER = gql`
  mutation CreateUser($name: String!, $email: String!, $password: String!) {
    createUser(input: { name: $name, email: $email, password: $password }) {
      success
    }
  }
`

const UserContext = React.createContext<UserContextValue | null>(null)

export const UserProvider: React.FC = props => {
  const firebase = useFirebase()
  const [createUser] = useMutation(CREATE_USER)
  const [isAuthenticated, setIsAuthenticated] = useState(false)

  const promiseFn = useCallback(
    () =>
      new Promise(resolve => {
        firebase.auth().onAuthStateChanged(user => resolve(Boolean(user)))
      }),
    []
  )

  const { data, isPending } = useAsync({ promiseFn })

  useEffect(() => {
    if (!isPending) {
      setIsAuthenticated(data as boolean)
    }
  }, [isPending])

  const login = async (email: string, password: string) => {
    try {
      await firebase.auth().signInWithEmailAndPassword(email, password)
      setIsAuthenticated(true)
      navigate('/')
    } catch (err) {
      throw new Error(
        `Error logging in with email ${email} and password ${password}.`
      )
    }
  }

  const logout = async () => {
    try {
      await firebase.auth().signOut()
      setIsAuthenticated(false)
      navigate('/login')
    } catch (err) {
      throw new Error(`Error logging out.`)
    }
  }

  const register = async (name: string, email: string, password: string) => {
    try {
      await createUser({ variables: { name, email, password } })
      setIsAuthenticated(true)
      navigate('/calculator')
    } catch (err) {
      throw new Error(
        `Error registering with name ${name}, email ${email}, and password ${password}.`
      )
    }
  }

  return (
    <UserContext.Provider
      {...props}
      value={{ isAuthenticated, login, logout, register }}
    />
  )
}

export const useUser = () => {
  return useContextSafe(UserContext)
}
