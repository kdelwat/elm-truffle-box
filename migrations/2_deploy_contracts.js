// SimpleStorage deployer taken from Truffle's React box
// https://github.com/truffle-box/react-box/blob/master/migrations/2_deploy_contracts.js

var SimpleStorage = artifacts.require("./SimpleStorage.sol");

module.exports = function(deployer) {
  deployer.deploy(SimpleStorage);
};