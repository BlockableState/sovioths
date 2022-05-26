{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module MerkleTree where

import Crypto.Hash
import Data.Binary
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteString (ByteString, singleton)
import GHC.Generics

-- merkle Tree using (HashAlgorithm a)
data MerkleTree a ba
  = MtNode ba (MerkleTree a ba) (MerkleTree a ba)
  | Leaf ba
  deriving (Show, Generic)

deriving instance Binary (MerkleTree a ByteString)

fromList :: (HashAlgorithm a, ByteArray ba) => [ByteString] -> (MerkleTree a ba)
fromList xs = trees !! depth !! 0
  where
    leaves = map (Leaf . convert) $ xs ++ cycle [""]
    trees = iterate goUp leaves
    depth = snd $ head $ dropWhile ((< length xs) . fst) [(2 ^ n, n) | n <- [1 ..]]

getSig :: forall a ba. (HashAlgorithm a, ByteArray ba) => (MerkleTree a ba) -> ba
getSig (MtNode sig _ _) = sig
getSig (Leaf x) = hashB @a x

goUp :: (HashAlgorithm a, ByteArray ba) => [(MerkleTree a ba)] -> [(MerkleTree a ba)]
goUp (a : b : xs) = merge a b : goUp xs
goUp xs = xs

merge :: (HashAlgorithm a, ByteArray ba) => (MerkleTree a ba) -> MerkleTree a ba -> MerkleTree a ba
merge a b = MtNode (hashTwoNodes a b) a b

-- hash a node value
hashNode :: forall a ba. (HashAlgorithm a, ByteArray ba) => MerkleTree a ba -> ba
hashNode x = hashB @a $ getSig x

hashTwoNodes :: forall a ba. (HashAlgorithm a, ByteArray ba) => MerkleTree a ba -> MerkleTree a ba -> ba
hashTwoNodes a b = hashB @a (hashNode a <> hashNode b)

verify :: (HashAlgorithm a, ByteArray ba) => MerkleTree a ba -> Bool
verify (MtNode sig l r) = hashTwoNodes l r == sig && verify l && verify r
verify _ = True

-- helpers
hashB :: forall a ba bb. (ByteArray ba, HashAlgorithm a, ByteArray bb) => ba -> bb
hashB = convert . (hash @ba @a)

getLeafValues :: MerkleTree a ba -> [ba]
getLeafValues (Leaf a) = [a]
getLeafValues (MtNode _ a b) = getLeafValues a <> getLeafValues b