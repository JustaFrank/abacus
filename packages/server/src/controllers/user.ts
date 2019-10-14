import { Application } from '../app'
import {
  CreateUserInput,
  UpdateUserInput,
  DeleteUserInput,
  User
} from '../schemas/user'
import { error } from '../server-error'

const USER_COLLECTION = 'users'

export const getUser = async (
  { db }: Application,
  id: string
): Promise<User> => {
  try {
    const snapshot = await db
      .collection(USER_COLLECTION)
      .doc(id)
      .get()
    return { ...snapshot.data(), id } as User
  } catch (err) {
    throw error(`Error getting user with id ${id}.`, err)
  }
}

export const createUser = async (
  { auth, db }: Application,
  { name, email, password }: CreateUserInput
): Promise<void> => {
  try {
    const userRecord = await auth.createUser({ email, password })
    await db
      .collection(USER_COLLECTION)
      .doc(userRecord.uid)
      .set({ name, addedFunctions: [], createdFunctions: [] })
  } catch (err) {
    throw error(`Error creating user with name ${name}.`, err)
  }
}

export const updateUser = async (
  { db }: Application,
  { id, ...updates }: UpdateUserInput
): Promise<void> => {
  try {
    await db
      .collection(USER_COLLECTION)
      .doc(id)
      .update(updates)
  } catch (err) {
    throw error(`Error updating user with id ${id}.`, err)
  }
}

export const deleteUser = async (
  { auth, db }: Application,
  { id }: DeleteUserInput
): Promise<void> => {
  try {
    auth.deleteUser(id)
    db.collection(USER_COLLECTION)
      .doc(id)
      .delete()
  } catch (err) {
    throw error(`Error deleting user with id ${id}.`, err)
  }
}
