{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import MerkleTree
import Crypto.Hash
import BlockChain
import Data.ByteString hiding (last)
import Data.Binary (Word8, decode)
import Data.String


main :: IO ()
main = do
    let xs :: [[ByteString]] = Prelude.map (return . fromString . show) [1..10]
    let bc = Prelude.foldr (addBlock @SHA256 "0" "0") Nil xs
    print bc
    print $ mapBlockChain getBlockData bc
    let newBlock = searchNonce 2 <$> lastBlock bc
    print newBlock
    print $ unpack . hashBlock . snd <$> newBlock

    
