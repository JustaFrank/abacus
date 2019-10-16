import React from 'react'
import { useMutation } from '@apollo/react-hooks'
import gql from 'graphql-tag'

import { useFirebase } from './firebase-context'
import { useContextSafe } from '../hooks/use-context-safe'
import { useUser } from './user-context'
import { CustomFunction } from './marketplace-context'

interface LibraryContextValue {
  functions: CustomFunction[]
  removeFunction: (functionID: string) => void
}

const LibraryContext = React.createContext<LibraryContextValue | null>(null)

const REMOVE_FUNCTION_FROM_USER = gql`
  mutation RemoveFunctionFromUser($id: String!, $functionID: String!) {
    removeFunctionFromUser(input: { id: $id, functionID: $functionID }) {
      success
    }
  }
`

export const LibraryProvider: React.FC = props => {
  const firebase = useFirebase()
  const { user, setAddedFunctions } = useUser()
  const [removeFunctionFromUser] = useMutation(REMOVE_FUNCTION_FROM_USER)

  const functions = user ? user.addedFunctions : []

  const removeFunction = async (functionID: string) => {
    const { uid } = await new Promise((resolve, reject) =>
      firebase
        .auth()
        .onAuthStateChanged(user => (user ? resolve(user) : reject()))
    )
    await removeFunctionFromUser({
      variables: {
        functionID,
        id: uid
      }
    })
    setAddedFunctions(functions.filter(({ id }) => id !== functionID))
  }

  return (
    <LibraryContext.Provider value={{ removeFunction, functions }} {...props} />
  )
}

export const useLibrary = () => {
  return useContextSafe(LibraryContext)
}
