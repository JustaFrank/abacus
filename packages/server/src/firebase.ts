import admin from 'firebase-admin'

interface FirebaseAdminConfig {
  projectId: string
  clientEmail: string
  privateKey: string
}

export const firebase = (config: FirebaseAdminConfig) => {
  const { projectId, ...credential } = config
  admin.initializeApp({
    projectId,
    credential: admin.credential.cert(credential)
  })
  return admin
}
