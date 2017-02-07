import TestClasses

class Default a => Bad a where
  aHasKindStarToStar :: a Int

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (3,7): Illegal type in context
--- |  referenced class : Default
--- |  which has kind   : *        -> Constraint
--- |  but used as if   : (* -> *) -> Constraint
