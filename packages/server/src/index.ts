import { ApolloServer } from 'apollo-server'

import { env } from './env'
import { firebase } from './firebase'
import { getResolvers } from './resolvers'
import { typeDefs } from './schema'

const admin = firebase({
  projectId: env.FIREBASE_PROJECT_ID,
  clientEmail: env.FIREBASE_CLIENT_EMAIL,
  privateKey: env.FIREBASE_PRIVATE_KEY
})

const app = { auth: admin.auth(), db: admin.firestore() }

const server = new ApolloServer({
  typeDefs,
  resolvers: getResolvers(app)
})

server
  .listen({ port: process.env.PORT || 8080 })
  .then(({ url }) => console.log(`🚀 Server listening at ${url}`))
