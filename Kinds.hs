{-# LANGUAGE
 PolyKinds,
 DataKinds,
 KindSignatures,
 RankNTypes,
 ExistentialQuantification,
 GADTs
 #-}

-- | I think this is an error.  
-- bad has a type which can't be written in haskell
module Kinds where


data ImpList = Z
             | forall a . N a ImpList

data Vec (a :: ImpList) where
  Nil :: Vec Z
  Cons :: a -> Vec i -> Vec (N a i)
  
bad = Cons undefined Nil