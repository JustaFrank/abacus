import { auth, firestore, app } from 'firebase-admin'

export const getResolvers = (auth: auth.Auth, db: firestore.Firestore) => ({
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
    async createUser(
      _: null,
      { email, password }: { email: string; password: string }
    ) {
      const userRecord = await auth.createUser({ email, password })
      console.log('Successfully created user:', userRecord.uid)
      return { success: true }
    }
  }
})
