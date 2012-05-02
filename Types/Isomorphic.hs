{-# LANGUAGE
 MultiParamTypeClasses,
 FunctionalDependencies
 #-}
module Types.Isomorphic ( Iso(..) 
                        ) where 

-- | @'Iso'@ is a class which allows for a proof that a is of "type" m
class Iso m a | a -> m where
  tp :: a
  val :: m a
  
  into :: a -> m a
  into = const val
  
  outof :: m a -> a
  outof = const tp