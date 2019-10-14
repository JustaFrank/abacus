import { Application } from '../app'

import { createUser, deleteUser, getUser, updateUser } from '../controllers'
import {
  CreateUserInput,
  DeleteUserInput,
  UpdateUserInput,
  User
} from '../schemas'

export const userResolvers = (app: Application) => ({
  Query: {
    async user(id: string): Promise<User> {
      return getUser(app, id)
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
    async deleteUser(_: null, { input }: { input: DeleteUserInput }) {
      await deleteUser(app, input)
      return { success: true }
    }
  }
})
