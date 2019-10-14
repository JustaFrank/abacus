import { gql } from 'apollo-server'

import { functionSchema } from './function'
import { userSchema } from './user'

export * from './function'
export * from './user'

const commonSchema = gql`
  type Query {
    root: String
  }

  type Mutation {
    root: String
  }

  type MutationResult {
    success: Boolean!
  }
`

export const typeDefs = [commonSchema, userSchema, functionSchema]
