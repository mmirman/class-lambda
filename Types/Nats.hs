{-# LANGUAGE
 MultiParamTypeClasses,
 GADTs,
 FlexibleContexts,
 StandaloneDeriving,
 FlexibleInstances,
 UndecidableInstances
 #-}
module Types.Nats 
       ( Nat(..)
       , Z()
       , S()
       ) where

import Types.Isomorphic 

               
-- | 'Nat' is a witness for types, further enforcing that we can not ever extend 
-- implement "get" with anything but S and Z.
data Nat b where
  Z :: Nat Z
  S :: Iso Nat a => Nat a -> Nat (S a)

deriving instance Show Z
deriving instance (Show a, Iso Nat a) => Show (S a)

instance (Iso Nat a, Show a) => Show (Nat a) where show a = show $ outof a
data Z where Zero :: Z
data S a where Succ :: Iso Nat a => a -> S a
               
instance Iso Nat Z where
  tp = Zero
  val = Z

instance Iso Nat a => Iso Nat (S a) where
  tp = Succ tp
  val = S val
