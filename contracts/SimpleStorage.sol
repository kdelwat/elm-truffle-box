// SimpleStorage example contract taken from Truffle's React box
// https://github.com/truffle-box/react-box/blob/master/contracts/SimpleStorage.sol

pragma solidity ^0.4.18;

contract SimpleStorage {
    uint storedData;

    function set(uint x) public {
        storedData = x;
    }

    function get() public view returns (uint) {
        return storedData;
    }
}