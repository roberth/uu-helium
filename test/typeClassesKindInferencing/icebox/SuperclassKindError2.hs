-- In Prelude:
{-class Functor f where
  fmap :: (a -> b) -> f a -> f b -}

class HigherK f where
  fmap :: f a

class HigherK a => Invalid a where
  aHasKindStar :: a

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (5,7): Illegal type in context                                                                                                            
--- |  referenced class : Functor                                 
--- |  which has kind   : (* -> *) -> Constraint
--- |  but used as if   : *        -> Constraint
