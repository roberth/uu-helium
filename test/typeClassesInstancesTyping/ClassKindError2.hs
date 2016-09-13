
class Invalid a where
  aHasKindstar :: a -> Bool
  aHasKindStarToStar :: a Int

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (3,19): Illegal type in type application
--- |  type             : a -> Bool
--- |  type constructor : ->
--- |    kind           : *        -> * -> *
--- |    does not match : (* -> *) -> * -> *
--- | 
