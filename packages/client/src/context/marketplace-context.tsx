import React, { useState, useEffect } from 'react'
import { useQuery, useMutation } from '@apollo/react-hooks'
import gql from 'graphql-tag'

import { FullPageSpinner } from '../components/common/FullPageSpinner'
import { useFirebase } from './firebase-context'
import { useContextSafe } from '../hooks/use-context-safe'
import { useUser } from './user-context'

interface MarketplaceContextValue {
  functions: CustomFunction[]
  addFunction: (functionID: string) => void
}

export interface CustomFunction {
  id: string
  name: string
  description: string
  body: string
  publisher: string
}

const MarketplaceContext = React.createContext<MarketplaceContextValue | null>(
  null
)

const GET_FUNCTIONS = gql`
  query GetFunctions {
    functions {
      id
      name
      description
      body
      publisher
    }
  }
`

const ADD_FUNCTION_TO_USER = gql`
  mutation AddFunctionToUser($id: String!, $functions: [String]!) {
    addFunctionToUser(input: { id: $id, functions: $functions }) {
      success
    }
  }
`

export const MarketplaceProvider: React.FC = props => {
  const firebase = useFirebase()
  const { user } = useUser()
  const { loading, error, data } = useQuery(GET_FUNCTIONS)
  const [addFunctionToUser] = useMutation(ADD_FUNCTION_TO_USER)

  const [functions, setFunctions] = useState<CustomFunction[]>([])

  useEffect(() => {
    if (data) {
      const userFunctions = user ? user.addedFunctions.map(({ id }) => id) : []
      const initialFunctions = data.functions.filter(
        ({ id }: CustomFunction) => !userFunctions.includes(id)
      )
      setFunctions(initialFunctions)
    }
  }, [user, data])

  if (loading) {
    return <FullPageSpinner color="#273746" backgroundColor="white" />
  } else if (error) {
    throw error
  }

  const addFunction = async (functionID: string) => {
    const { uid } = await new Promise((resolve, reject) =>
      firebase
        .auth()
        .onAuthStateChanged(user => (user ? resolve(user) : reject()))
    )
    await addFunctionToUser({
      variables: {
        id: uid,
        functions: `functions/${functionID}`
      }
    })
    setFunctions(functions.filter(({ id }) => id !== functionID))
  }

  return (
    <MarketplaceContext.Provider
      value={{ addFunction, functions }}
      {...props}
    />
  )
}

export const useMarketplace = () => {
  return useContextSafe(MarketplaceContext)
}
