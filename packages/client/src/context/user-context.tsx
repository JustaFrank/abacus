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
  setAddedFunctions: (functions: CustomFunction[]) => void
  publishFunction: (name: string, description: string, body: string) => void
  login: (email: string, password: string) => void
  logout: () => void
  register: (name: string, email: string, password: string) => void
}

interface User {
  id: string
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
        body
        description
      }
      createdFunctions {
        id
        name
        body
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

const CREATE_FUNCTION = gql`
  mutation CreateFunction(
    $name: String!
    $description: String!
    $body: String!
    $publisher: String!
  ) {
    createFunction(
      input: {
        name: $name
        description: $description
        body: $body
        publisher: $publisher
      }
    ) {
      success
    }
  }
`

const UserContext = React.createContext<UserContextValue | null>(null)

export const UserProvider: React.FC = props => {
  const [getUser, { data }] = useLazyQuery(GET_USER)
  const firebase = useFirebase()
  const [createUser] = useMutation(CREATE_USER)
  const [createFunction] = useMutation(CREATE_FUNCTION)
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

  const publishFunction = async (
    name: string,
    description: string,
    body: string
  ) => {
    try {
      await createFunction({
        variables: { name, description, body, publisher: user && user.id }
      })
    } catch (err) {
      throw new Error(`Error creating new function`)
    }
  }

  const setAddedFunctions = (functions: CustomFunction[]) => {
    user && setUser({ ...user, addedFunctions: functions })
  }

  return (
    <UserContext.Provider
      {...props}
      value={{
        user,
        login,
        logout,
        register,
        setAddedFunctions,
        publishFunction
      }}
    />
  )
}

export const useUser = () => {
  return useContextSafe(UserContext)
}
