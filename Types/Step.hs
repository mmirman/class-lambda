{-# LANGUAGE
 MultiParamTypeClasses,
 GADTs,
 FlexibleContexts,
 FunctionalDependencies,
 FlexibleInstances,
 UndecidableInstances
 #-}
module Types.Step (Step(), stepVar) where

import Types.Isomorphic
import Types.Terms
import Types.Nats

class (Iso Nat a, Iso Nat b, Iso Nat o) => Add a b o | a b -> o --where add :: Nat a -> Nat b -> Nat o
instance Iso Nat a => Add Z a a                                 --where add Z a = a
instance Add b a c => Add (S b) a (S c)                         --where add (S a) b = S (add a b)

class (Iso Nat w, Iso Term i, Iso Term o) => AddVars i w o | w i -> o        --where addVars :: Term i -> Nat w -> Term o
instance Add w' w w'' => AddVars (Var w') w (Var w'')                        --where addVars (Var w') w = Var $ add w' w
instance (AddVars l w l', AddVars r w r') => AddVars (App l r) w (App l' r') --where addVars (App l r) w = App (addVars l w) (addVars r w)
instance (AddVars e Z e') => AddVars (Lam e) Z (Lam e')                      --where addVars (Lam e) Z = Lam (addVars e Z)
instance (AddVars e w e', Iso Nat w) => AddVars (Lam e) (S w) (Lam e')       --where addVars (Lam e) (S w) = Lam (addVars e w)
instance (Iso Nat w) => AddVars (Val e) w (Val e)                            --where addVars (Val e) w = Val e

class (Iso Nat o, Iso Nat n, Iso Nat m, Iso Term i, Iso Nat w, Iso Term i') => SubV o n m i w i' | o n m i -> i'
instance (Iso Nat m, AddVars i w i') => SubV m Z Z i w i'
instance (Iso Nat m, Iso Nat a, Iso Term i, Iso Nat w) => SubV m (S a) Z i w (Var m)
instance (Iso Nat m, Iso Nat a, Iso Term i, Iso Nat w) => SubV m Z (S a) i w (Var m)
instance (SubV m a b i w i') => SubV m (S a) (S b) i w i'

class (Iso Nat n, Iso Term a, Iso Term i, Iso Term o, Iso Nat w) => Sub n a i w o | n a i -> o
--  where sub :: Nat n -> Term a -> Term i -> Nat w -> Term o

instance (SubV r n r i w i', Iso Nat w) => Sub n i (Var r) w i'
instance (Sub n i l w l', Sub n i r w r') => Sub n i (App l r) w (App l' r')
instance (Iso Term i, Iso Nat n, Iso Nat w) => Sub n i (Val r) w (Val r)
instance (Iso Nat w, Iso Nat n, Sub (S n) i e (S w) e') => Sub n i (Lam e) w (Lam e')

class (Iso Term a, Iso Term b) => Step a b | a -> b

instance Step (Val e) (Val e)
instance (Iso Nat e) => Step (Var e) (Var e)
instance (Iso Term e) => Step (Lam e) (Lam e)

instance ( Step ef (Lam e) 
         , Sub Z v e Z o
         , Step o o'
         ) => Step (App ef v) o'

stepVar :: (Step a b) => Term a -> Term b
stepVar _ = val
