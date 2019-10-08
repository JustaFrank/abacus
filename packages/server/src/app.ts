import { auth, firestore } from 'firebase-admin'

export interface Application {
  auth: auth.Auth
  db: firestore.Firestore
}
