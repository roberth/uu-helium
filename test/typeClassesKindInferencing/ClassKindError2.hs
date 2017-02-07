
class Invalid a where
  aHasKindstar :: a -> Bool
  aHasKindStarToStar :: a Int

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (4,25): Illegal type in type application
--- |  type             : a Int
--     FIXME: Blaming the -> kind is horrible...
--- |  type constructor : a
--- |    kind           : *
--- |    does not match : * -> *
--- | 
