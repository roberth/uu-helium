
class Show a => Invalid a where
  aHasKindStarToStar :: a Int

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (2,7): Illegal type in context                                                                                                            
--- |  referenced class : Show                                 
--- |  actual kind      : * -> Constraint
--- |  used as kind     : a -> Constraint
