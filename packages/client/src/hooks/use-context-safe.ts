import { useContext } from 'react'

export const useContextSafe = <T>(
  context: React.Context<T | null>,
  label?: string
): T => {
  const value = useContext(context)
  if (value === null) {
    throw new Error(`Missing context provider${label ? ` for ${label}` : ''}`)
  }
  return value
}
