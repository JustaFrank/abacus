import { gql } from 'apollo-server'

export const typeDefs = gql`
  type Query {
    user(id: String!): User!
    functions: [Function]!
  }

  type Mutation {
    createUser(email: String!, password: String!): CreateUserResponse!
    addFunctionToUser(
      userID: String!
      functionID: String!
    ): AddFunctionToUserResponse
  }

  type CreateUserResponse {
    success: Boolean!
    user: User!
  }

  type AddFunctionToUserResponse {
    success: Boolean!
    user: User!
  }

  type User {
    id: ID!
    name: String!
    functions: [String]!
  }

  type Function {
    id: ID!
    publisher: String!
    body: String!
  }
`
