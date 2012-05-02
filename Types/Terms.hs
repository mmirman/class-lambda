{-# LANGUAGE
 MultiParamTypeClasses,
 GADTs,
 FlexibleContexts,
 StandaloneDeriving,
 UndecidableInstances,
 FlexibleInstances
 #-}
module Types.Terms
       ( Term(..)
       , App()
       , Var()         
       , Lam()
       , Val()         
       ) where

import Types.Isomorphic
import Types.Nats

data Term a where
  App :: (Iso Term a, Iso Term b) => Term a -> Term b -> Term (App a b)
  Var :: Iso Nat a => Nat a -> Term (Var a)
  Lam :: Iso Term a => Term a -> Term (Lam a)
  Val :: a -> Term (Val a)
  
deriving instance (Show a, Show b) => Show (App a b)
deriving instance (Show a) => Show (Lam a)
instance Show (Val a) where show _ = "Val"
deriving instance (Show a) => Show (Var a)
instance (Iso Term a, Show a) => Show (Term a) where show a = show $ outof a


data App a b where Apply :: (Iso Term a, Iso Term b) => a -> b -> App a b
instance (Iso Term a, Iso Term b) => Iso Term (App a b) where
  val = App val val
  tp = Apply tp tp
  
  into (Apply a b) = App (into a) (into b)
  outof (App a b) = Apply (outof a) (outof b)
  
data Var a where Variable :: Iso Nat a => a -> Var a  
instance (Iso Nat a) => Iso Term (Var a) where
  val = Var val
  tp = Variable tp
  
  into (Variable a) = Var (into a)
  outof (Var a) = Variable (outof a)
  
data Lam a where Lambda :: Iso Term a => a -> Lam a
instance (Iso Term a) => Iso Term (Lam a) where
  val = Lam val
  tp = Lambda tp
  
  into (Lambda a) = Lam (into a)
  outof (Lam a) = Lambda (outof a)  

data Val a where Value :: a -> Val a
instance Iso Term (Val a) where
  val = Val undefined
  tp = Value undefined
  
  into (Value a) = Val a
  outof (Val a) = Value a
