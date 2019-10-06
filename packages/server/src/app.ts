import { ApolloServer, ApolloError, ValidationError, gql } from 'apollo-server'

interface User {
  id: string
  name: string
  functions: string[]
}

interface Function {
  publisher: string
  body: string
}

const typeDefs = gql`
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

  type Query {
    user(id: String!): User
    functions: [Function]
  }
`

const resolvers = {
  Query: {
    async user(_: null, { id }: { id: string }) {
      return { id, name: 'Test', functions: [] }
    },
    async functions() {
      return [
        {
          id: '12341234234',
          publisher: 'asdf',
          body: 'function test(a, b) { return a + b }'
        }
      ]
    }
  }
}

const server = new ApolloServer({ typeDefs, resolvers })

server
  .listen({ port: process.env.PORT || 8080 })
  .then(({ url }) => console.log(`Server listening at ${url}`))
