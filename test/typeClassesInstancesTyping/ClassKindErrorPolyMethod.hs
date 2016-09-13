
class Invalid x where
  aHasKindstar :: x -> b
  aHasKindStarToStar :: b -> x Int

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (3,19): Illegal type in type application
--- |  type             : x -> b
--- |  type constructor : ->
--- |    kind           : *        -> * -> *
--- |    does not match : (* -> *) -> a -> *
--- | 
