import { gql } from 'apollo-server'

export interface CustomFunction {
  id: string
  name: string
  body: string
  publisher: string
  testCases: TestCase[]
}

export interface TestCase {
  inputs: number[]
  output: number[]
}

export interface CreateFunctionInput {
  name: string
  description: string
  body: string
  publisher: string
}

export interface UpdateFunctionInput {
  id: string
  name?: string
  body?: string
  testCases?: TestCase[]
}

export interface DeleteFunctionInput {
  id: string
}

export const functionSchema = gql`
  extend type Query {
    functions: [Function]!
    function(id: String!): Function!
  }

  extend type Mutation {
    createFunction(input: CreateFunctionInput!): MutationResult!
    updateFunction(input: UpdateFunctionInput!): MutationResult!
    deleteFunction(input: DeleteFunctionInput!): MutationResult!
  }

  type Function {
    id: ID!
    name: String!
    description: String!
    body: String!
    publisher: String!
    testCases: [TestCase]!
  }

  type TestCase {
    inputs: [Float]!
    output: Float!
  }

  input CreateFunctionInput {
    name: String!
    description: String!
    body: String!
    publisher: String!
  }

  input UpdateFunctionInput {
    name: String
    body: String
    testCases: [InputTestCase]
  }

  input InputTestCase {
    inputs: [Float]!
    output: Float!
  }

  input DeleteFunctionInput {
    id: String!
  }
`
