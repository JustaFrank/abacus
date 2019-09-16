import { useContext } from 'react'
import { MissingContextProvider } from './error'

export const useContextSafe = (context: React.Context<any | null>) => {
  const value = useContext(context)
  if (value === null) {
    throw MissingContextProvider
  }
  return value
}
