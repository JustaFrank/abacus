import { UserArguments, createNewUser } from './controllers/user'
import { Application } from './app'

export const getResolvers = (app: Application) => {
  const { db } = app
  return {
    Query: {
      async user(_: null, { id }: { id: string }) {
        const snapshot = await db
          .collection('users')
          .doc(id)
          .get()
        const doc = snapshot.data()
        return doc
      },
      async functions() {
        const snapshot = await db.collection('functions').get()
        const docs = snapshot.docs.map(doc => doc.data())
        return docs
      }
    },
    Mutation: {
      async createUser(_: null, args: UserArguments) {
        await createNewUser(app, args)
        return { success: true }
      }
    }
  }
}
