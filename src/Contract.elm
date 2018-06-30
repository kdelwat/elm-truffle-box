module Contract exposing (get, set)

import Eth.Units
import Evm.Decode as Evm
import Evm.Encode as Evm
import Eth.Types exposing (Call, Address)
import BigInt exposing (BigInt)

get : Address -> Call BigInt
get contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "get()" []
    , nonce = Nothing
    , decoder = Evm.toElmDecoder Evm.uint}

set : Address -> Maybe Address -> BigInt -> Call Bool
set contractAddress account value =
    { to = Just contractAddress
    , from = account
    , gas = Nothing
    , gasPrice = Just <| Eth.Units.gwei 4
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "set(uint256)" [Evm.UintE value]
    , nonce = Nothing
    , decoder = Evm.toElmDecoder Evm.bool
    }

