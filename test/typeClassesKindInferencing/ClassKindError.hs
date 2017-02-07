
class Invalid a where
  aHasKindstar :: a
  aHasKindStarToStar :: a Int

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (3,3): Illegal type in type signature
--- |  type             : a
--- |    kind           : * -> *
--- |    expected kind  : *
--- | 
