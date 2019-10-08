import { Application } from '../app'
import { error } from '../server-error'

export interface UserArguments {
  email: string
  name: string
  password: string
}

const USER_COLLECTION = 'users'

export const createNewUser = async (
  { auth, db }: Application,
  { email, name, password }: UserArguments
) => {
  try {
    const userRecord = await auth.createUser({ email, password })
    await db
      .collection(USER_COLLECTION)
      .doc(userRecord.uid)
      .set({ name, functions: [] })
  } catch (err) {
    throw error(`Error creating user with name ${name}.`, err)
  }
}
