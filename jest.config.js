/*
* For a detailed explanation regarding each configuration property, visit:
* https://jestjs.io/docs/configuration
*/

const { setupJestEnv } = require("/home/joe/Documents/Tezos/Dapps/RaffleDapp/node_modules/create-tezos-smart-contract/dist/modules/jest/env.js");

const config = {
 coverageProvider: "v8",

 // The maximum amount of workers used to run your tests. Can be specified as % or a number. E.g. maxWorkers: 10% will use 10% of your CPU amount + 1 as the maximum worker number. maxWorkers: 2 will use a maximum of 2 workers.
 maxWorkers: "1",

 // The paths to modules that run some code to configure or set up the testing environment before each test
 setupFiles: [
   "/home/joe/Documents/Tezos/Dapps/RaffleDapp/node_modules/create-tezos-smart-contract/dist/modules/jest/globals.js"
 ],
};

module.exports = async () => {
 const globals = await setupJestEnv();

 return {
   ...config,
   globals: {
     ...config.globals,
     ...globals,
   }
 };
};

