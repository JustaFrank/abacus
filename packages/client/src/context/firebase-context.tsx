import React from 'react'

import firebase from 'firebase/app'
import 'firebase/auth'

import { useContextSafe } from '../hooks/use-context-safe'

type FirebaseContextValue = typeof firebase

const FirebaseContext = React.createContext<FirebaseContextValue | null>(null)

export const FirebaseProvider: React.FC = props => {
  firebase.initializeApp({
    apiKey: 'AIzaSyAQ2Jlo3YWqPsh9e5DYvFuUhr5XCvKYiOE',
    authDomain: 'abacus-41130.firebaseapp.com',
    databaseURL: 'https://abacus-41130.firebaseio.com',
    projectId: 'abacus-41130',
    storageBucket: 'abacus-41130.appspot.com',
    messagingSenderId: '901325953978',
    appId: '1:901325953978:web:a9c8fbaf3c41cd89d41b83',
    measurementId: 'G-CQ8Q111M64'
  })

  return <FirebaseContext.Provider {...props} value={firebase} />
}

export const useFirebase = () => {
  return useContextSafe(FirebaseContext)
}
