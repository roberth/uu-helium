
class Show a => Valid a where
  aHasKindStar :: a

class Functor m where
  map' :: (a -> b) -> m a -> m b
  
class Functor m => Applicative m where
  ap :: m (a -> b) -> m a -> m b
  
--- option: --overloading
--- option: --kind-inferencing
--- ok
