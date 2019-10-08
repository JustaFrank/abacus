export interface ServerError {
  message: string
  data: any
}

export const error = (message: string, data: any = {}) => {
  return { message, data }
}
