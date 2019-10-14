import { Application } from '../app'

import {
  createFunction,
  deleteFunction,
  getAllFunctions,
  updateFunction
} from '../controllers'
import {
  CreateFunctionInput,
  CustomFunction,
  DeleteFunctionInput,
  UpdateFunctionInput
} from '../schemas'

export const functionResolvers = (app: Application) => ({
  Query: {
    async functions(): Promise<CustomFunction[]> {
      const res = await getAllFunctions(app)
      console.log(res)
      return res
    }
  },
  Mutation: {
    async createFunction(_: null, { input }: { input: CreateFunctionInput }) {
      await createFunction(app, input)
      return { success: true }
    },
    async updateFunction(_: null, { input }: { input: UpdateFunctionInput }) {
      await updateFunction(app, input)
      return { success: true }
    },
    async deleteFunction(_: null, { input }: { input: DeleteFunctionInput }) {
      await deleteFunction(app, input)
      return { success: true }
    }
  }
})
