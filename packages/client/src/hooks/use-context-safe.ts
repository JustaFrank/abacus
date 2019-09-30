import { useContext } from 'react'

export const useContextSafe = (
  context: React.Context<any | null>,
  label?: string
) => {
  const value = useContext(context)
  if (value === null) {
    throw new Error(`Missing context provider${label ? ` for ${label}` : ''}`)
  }
  return value
}
