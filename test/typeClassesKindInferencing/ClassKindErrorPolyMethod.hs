
class Invalid x where
  aHasKindstar :: x -> b
  aHasKindStarToStar :: b -> x Int

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (4,30): Illegal type in type application
--- |  type             : x Int
--- |  type constructor : x
--- |    kind           : *
--- |    does not match : * -> *
--- | 
