interface AbacusError {
  type: string
  message?: string
}

export const MissingContextProvider: AbacusError = {
  type: 'MissingContextProvider'
}
