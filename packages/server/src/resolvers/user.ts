import { Application } from '../app'

import {
  createUser,
  deleteUser,
  getUser,
  updateUser,
  addFunctionToUser
} from '../controllers'
import {
  CreateUserInput,
  DeleteUserInput,
  UpdateUserInput,
  User,
  AddFunctionToUserInput
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
    async deleteUser(_: null, { input }: { input: DeleteUserInput }) {
      await deleteUser(app, input)
      return { success: true }
    }
  }
})
