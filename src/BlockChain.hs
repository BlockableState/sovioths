{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module BlockChain where

import Crypto.Hash
import Data.ByteString hiding (filter, head)
import MerkleTree
import Prelude hiding (take, replicate)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BL

data Block a = Block
  { prevHash :: ByteString,
    timeStamp :: ByteString,
    txRoot :: MerkleTree a ByteString,
    nonce :: ByteString
  } deriving Show

data BlockChain a = (Block a) :< (BlockChain a) | Nil deriving (Show)

mapBlockChain :: (Block a -> c) -> BlockChain a ->  [c]
mapBlockChain f (x:<xs) = f x : mapBlockChain f xs
mapBlockChain _ _ = []

infixr 5 :<

getBlockData :: Block a -> ByteString
getBlockData = Data.ByteString.concat . getLeafValues . txRoot

lastBlock :: BlockChain a -> Maybe (Block a)
lastBlock (x :< _) = Just x
lastBlock _ = Nothing


-------------------
-- Block validation
-------------------
hashBlock :: forall a. (HashAlgorithm a) => Block a -> ByteString
hashBlock bl = hashB @a (prevHash bl <> timeStamp bl <> getSig (txRoot bl) <> nonce bl)

verifyBlock :: forall a. (HashAlgorithm a) => Block a -> Bool
verifyBlock = verify . txRoot

verifyBlockPrev :: forall a. (HashAlgorithm a) => Block a -> Block a -> Bool
verifyBlockPrev a b = prevHash b == hashBlock a

verifyBlockChain :: forall a. (HashAlgorithm a) => BlockChain a -> Bool
verifyBlockChain (a :< b :< xs) = verifyBlockPrev b a && verifyBlock a && verifyBlock b && verifyBlockChain (b :< xs)
verifyBlockChain (a :< Nil) = verifyBlock a
verifyBlockChain Nil = True

-------------------
-- Nonce 
-------------------

verifyBlockNonce :: forall a. (HashAlgorithm a) => Int -> Block a -> Bool
verifyBlockNonce n bl = take n (hashBlock bl) == replicate n 0

addBlock :: HashAlgorithm a => ByteString -> ByteString -> [ByteString] -> BlockChain a -> BlockChain a
addBlock nc t txs xss@(x:<_) = Block (hashBlock x) t (fromList txs) nc:< xss
addBlock nc t txs Nil = Block "" t (fromList txs) nc:< Nil

-- compute a block with correct nonce by difficulty
searchNonce :: forall a. HashAlgorithm a => Int -> Block a -> (Int, Block a)
searchNonce df (Block a b c _) =  head $ filter (verifyBlockNonce df . snd) $ mkB <$> [1..]
  where 
    mkB n =  (n, Block a b c (toStrict $ encode n))