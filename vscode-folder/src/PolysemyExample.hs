{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module PolysemyExample where

import Data.ByteString
import Polysemy (Member, Sem)
import Polysemy.Internal as P
import Polysemy.KVStore

-- A few type definitions to get started
newtype Username = Username ByteString
newtype Password = Password ByteString
newtype PasswordHash = PasswordHash ByteString

data CryptoHash m a where
  -- | Generates a hash from a password
  MakeHash :: Password -> CryptoHash m PasswordHash
  -- | Check if a password matches a hash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

makeHash :: Member CryptoHash r => Password -> Sem r PasswordHash
makeHash x = send (MakeHash x)

validateHash :: Member CryptoHash r => Password -> PasswordHash -> Sem r Bool
validateHash password hash = send (ValidateHash password hash :: CryptoHash (Sem r) Bool)

addUser :: Members [CryptoHash, KVStore Username PasswordHash] r
        => Username
        -> Password
        -> Sem r ()
addUser username password = do
    hashedPassword <- makeHash password
    writeKV username hashedPassword

validatePassword :: Members [CryptoHash, KVStore Username PasswordHash] r
                 => Username
                 -> Password
                 -> Sem r Bool
validatePassword username password = do
    hashInStore <- lookupKV username
    case hashInStore of
      Just h  -> validateHash password h
      Nothing -> return False