import { gql } from 'apollo-server'

import { CustomFunction } from './function'

export interface User {
  id: string
  name: string
  addedFunctions: CustomFunction[]
  createdFunctions: CustomFunction[]
}

export interface CreateUserInput {
  name: string
  email: string
  password: string
}

export interface UpdateUserInput {
  id: string
  name?: string
  addedFunctions?: string[]
  createdFunctions?: string[]
}

export interface AddFunctionToUserInput {
  id: string
  functions: string[]
}

export interface DeleteUserInput {
  id: string
}

export const userSchema = gql`
  extend type Query {
    user(id: String!): User!
  }

  extend type Mutation {
    createUser(input: CreateUserInput!): MutationResult!
    updateUser(input: UpdateUserInput!): MutationResult!
    addFunctionToUser(input: AddFunctionToUserInput!): MutationResult!
    deleteUser(input: DeleteUserInput!): MutationResult!
  }

  type User {
    id: ID!
    name: String!
    addedFunctions: [Function]!
    createdFunctions: [Function]!
  }

  input CreateUserInput {
    email: String!
    name: String!
    password: String!
  }

  input UpdateUserInput {
    id: String!
    name: String
    addedFunctions: [String]
    createdFunctions: [String]
  }

  input AddFunctionToUserInput {
    id: String!
    functions: [String]!
  }

  input DeleteUserInput {
    id: String!
  }
`
