import envalid, { email, makeValidator, str } from 'envalid'

const sanitizeFirebaseKey = makeValidator<string>(key =>
  key.replace(/\\n/g, '\n')
)

export const env = envalid.cleanEnv(process.env, {
  FIREBASE_PROJECT_ID: str(),
  FIREBASE_CLIENT_EMAIL: email(),
  FIREBASE_PRIVATE_KEY: sanitizeFirebaseKey()
})
