import { Application } from '../app'
import { functionResolvers } from './function'
import { userResolvers } from './user'

export * from './function'
export * from './user'

export const resolvers = (app: Application) => [
  userResolvers(app),
  functionResolvers(app)
]
