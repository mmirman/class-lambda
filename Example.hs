{-# LANGUAGE
 FlexibleContexts,
 TypeOperators
 #-}
module Example where

import Types.Nats
import Types.Step
import Types.Terms

import Types.Isomorphic



true = Lam $ Lam $ Var $ S Z
false = Lam $ Lam $ Var $ Z

ifThenElse a b c = App (App a b) c

land a b = ifThenElse a b false
lor a b = App (ifThenElse a (Lam $ true) (Lam $ Var Z)) b

-- magic. try :t infer_implicit
infer_implicit = stepVar $ App (Lam $ Var Z) (lor false true)

-- Type operators make the heart grow warmer! 
infixr 0 :$
type m :$ a = m a

-- EVEN MORE MAGIC
infer_explicit = val :: (Step (App (Lam :$ Var Z) :$ Var :$ S :$ S Z) b => Term b)

main = do
  putStrLn $ show infer_implicit
  putStrLn $ show infer_explicit