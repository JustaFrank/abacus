import React, { useState, useEffect, useCallback } from 'react'
import { useMutation, useLazyQuery } from '@apollo/react-hooks'
import gql from 'graphql-tag'
import { navigate } from '@reach/router'
import { useAsync } from 'react-async'

import { FullPageSpinner } from '../components/common/FullPageSpinner'
import { useContextSafe } from '../hooks/use-context-safe'
import { useFirebase } from './firebase-context'
import { CustomFunction } from './marketplace-context'

interface UserContextValue {
  user: User | null
  login: (email: string, password: string) => void
  logout: () => void
  register: (name: string, email: string, password: string) => void
}

interface User {
  name: string
  addedFunctions: CustomFunction[]
  createdFunctions: CustomFunction[]
}

const GET_USER = gql`
  query GetUser($id: String!) {
    user(id: $id) {
      id
      name
      addedFunctions {
        id
        name
        description
      }
      createdFunctions {
        id
        name
        description
      }
    }
  }
`

const CREATE_USER = gql`
  mutation CreateUser($name: String!, $email: String!, $password: String!) {
    createUser(input: { name: $name, email: $email, password: $password }) {
      success
    }
  }
`

const UserContext = React.createContext<UserContextValue | null>(null)

export const UserProvider: React.FC = props => {
  const [getUser, { data }] = useLazyQuery(GET_USER)
  const firebase = useFirebase()
  const [createUser] = useMutation(CREATE_USER)
  const [user, setUser] = useState<User | null>(null)

  const fetchData = useCallback(async () => {
    const user = await new Promise<{ uid: string } | null>(resolve => {
      firebase.auth().onAuthStateChanged(user => resolve(user))
    })
    if (user) {
      await getUser({ variables: { id: user.uid } })
    }
  }, [firebase, getUser])

  const { isPending } = useAsync({ promiseFn: fetchData })

  useEffect(() => {
    if (data && data.user) {
      setUser(data.user)
    }
  }, [data])

  if (isPending) {
    return <FullPageSpinner />
  }

  const login = async (email: string, password: string) => {
    try {
      const { user } = await firebase
        .auth()
        .signInWithEmailAndPassword(email, password)
      await getUser({ variables: { id: user!.uid } })
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
      setUser(null)
      navigate('/login')
    } catch (err) {
      throw new Error(`Error logging out.`)
    }
  }

  const register = async (name: string, email: string, password: string) => {
    try {
      await createUser({ variables: { name, email, password } })
      await login(email, password)
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
      value={{ user, login, logout, register }}
    />
  )
}

export const useUser = () => {
  return useContextSafe(UserContext)
}
