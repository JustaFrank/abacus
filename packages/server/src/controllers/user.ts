import { Application } from '../app'
import {
  CreateUserInput,
  UpdateUserInput,
  DeleteUserInput,
  User,
  AddFunctionToUserInput,
  RemoveFunctionFromUserInput
} from '../schemas/user'
import { error } from '../server-error'
import admin = require('firebase-admin')

interface RawUser {
  id: string
  name: string
  addedFunctions: string[]
  createdFunctions: string[]
}

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
    const user = { ...snapshot.data(), id } as RawUser
    const addedFunctionsSnapshots = user.addedFunctions.length
      ? await db.getAll(...user.addedFunctions.map(path => db.doc(path)))
      : []
    const addedFunctions = addedFunctionsSnapshots.map(doc => ({
      ...doc.data(),
      id: doc.id
    })) as any[]
    const createdFunctionsSnapshots = user.createdFunctions.length
      ? await db.getAll(...user.createdFunctions.map(path => db.doc(path)))
      : []
    const createdFunctions = createdFunctionsSnapshots.map(doc => ({
      ...doc.data(),
      id: doc.id
    })) as any[]
    return { ...user, addedFunctions, createdFunctions }
  } catch (err) {
    console.log(err)
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

export const addFunctionToUser = async (
  { db }: Application,
  { id, functions }: AddFunctionToUserInput
) => {
  try {
    await db
      .collection(USER_COLLECTION)
      .doc(id)
      .update({
        addedFunctions: admin.firestore.FieldValue.arrayUnion(...functions)
      })
  } catch (err) {
    throw error(`Error adding functions to user with id ${id}.`, err)
  }
}

export const removeFunctionFromUser = async (
  { db }: Application,
  { id, functionID }: RemoveFunctionFromUserInput
) => {
  try {
    await db
      .collection(USER_COLLECTION)
      .doc(id)
      .update({
        addedFunctions: admin.firestore.FieldValue.arrayRemove(
          `functions/${functionID}`
        )
      })
  } catch (err) {
    throw error(`Error removing function from user with id ${id}.`, err)
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
