import { Application } from '../app'
import {
  CustomFunction,
  CreateFunctionInput,
  UpdateFunctionInput,
  DeleteFunctionInput
} from '../schemas'
import { error } from '../server-error'

const FUNCTION_COLLECTION = 'functions'

export const getFunction = async (
  { db }: Application,
  id: string
): Promise<CustomFunction> => {
  try {
    const snapshot = await db
      .collection(FUNCTION_COLLECTION)
      .doc(id)
      .get()
    return { ...snapshot.data(), id } as CustomFunction
  } catch (err) {
    throw error(`Error getting function with id ${id}.`, err)
  }
}

export const getAllFunctions = async ({
  db
}: Application): Promise<CustomFunction[]> => {
  try {
    const snapshot = await db.collection(FUNCTION_COLLECTION).get()
    const docs = snapshot.docs.map(doc => ({ ...doc.data, id: doc.id }))
    return docs as CustomFunction[]
  } catch (err) {
    throw error('Error getting all functions.')
  }
}

export const createFunction = async (
  { db }: Application,
  input: CreateFunctionInput
): Promise<void> => {
  try {
    await db.collection(FUNCTION_COLLECTION).add(input)
  } catch (err) {
    throw error(`Error creating function with name ${input.name}.`, err)
  }
}

export const updateFunction = async (
  { db }: Application,
  { id, ...updates }: UpdateFunctionInput
): Promise<void> => {
  try {
    await db
      .collection(FUNCTION_COLLECTION)
      .doc(id)
      .update(updates)
  } catch (err) {
    throw error(`Error updating function with id ${id}.`, err)
  }
}

export const deleteFunction = async (
  { db }: Application,
  { id }: DeleteFunctionInput
): Promise<void> => {
  try {
    db.collection(FUNCTION_COLLECTION)
      .doc(id)
      .delete()
  } catch (err) {
    throw error(`Error deleting function with id ${id}.`, err)
  }
}
