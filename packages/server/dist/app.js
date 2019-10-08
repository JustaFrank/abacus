"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var apollo_server_1 = require("apollo-server");
var env_1 = require("./env");
var firebase_1 = require("./firebase");
var resolvers_1 = require("./resolvers");
var schema_1 = require("./schema");
var admin = firebase_1.firebase({
    projectId: env_1.env.FIREBASE_PROJECT_ID,
    clientEmail: env_1.env.FIREBASE_CLIENT_EMAIL,
    privateKey: env_1.env.FIREBASE_PRIVATE_KEY
});
var auth = admin.auth();
var db = admin.firestore();
var server = new apollo_server_1.ApolloServer({
    typeDefs: schema_1.typeDefs,
    resolvers: resolvers_1.getResolvers(auth, db)
});
server
    .listen({ port: process.env.PORT || 8080 })
    .then(function (_a) {
    var url = _a.url;
    return console.log("\uD83D\uDE80 Server listening at " + url);
});
