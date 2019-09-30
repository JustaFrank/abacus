import { useContext } from 'react'

export const useContextSafe = (context: React.Context<any | null>) => {
  const value = useContext(context)
  if (value === null) {
    throw new Error('Missing context provider')
  }
  return value
}
