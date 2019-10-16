import { Application } from '../app'

import {
  addFunctionToUser,
  createUser,
  deleteUser,
  getUser,
  removeFunctionFromUser,
  updateUser
} from '../controllers'
import {
  AddFunctionToUserInput,
  CreateUserInput,
  DeleteUserInput,
  RemoveFunctionFromUserInput,
  UpdateUserInput,
  User
} from '../schemas'

export const userResolvers = (app: Application) => ({
  Query: {
    async user(_: null, { id }: { id: string }): Promise<User> {
      const user = await getUser(app, id)
      return user
    }
  },
  Mutation: {
    async createUser(_: null, { input }: { input: CreateUserInput }) {
      await createUser(app, input)
      return { success: true }
    },
    async updateUser(_: null, { input }: { input: UpdateUserInput }) {
      await updateUser(app, input)
      return { success: true }
    },
    async addFunctionToUser(
      _: null,
      { input }: { input: AddFunctionToUserInput }
    ) {
      await addFunctionToUser(app, input)
      return { success: true }
    },
    async removeFunctionFromUser(
      _: null,
      { input }: { input: RemoveFunctionFromUserInput }
    ) {
      await removeFunctionFromUser(app, input)
      return { success: true }
    },
    async deleteUser(_: null, { input }: { input: DeleteUserInput }) {
      await deleteUser(app, input)
      return { success: true }
    }
  }
})
