export interface ServerError {
  message: string
  data: any
}

export const error = (message: string, data: any = {}) => {
  console.log(JSON.stringify(data))
  return { message, data }
}
