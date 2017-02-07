import TestClasses

class Default a => Valid a where
  aHasKindStar :: a

--class Wildcard a => WildCardInt a where
--  giveInt :: a Int

--- option: --overloading
--- option: --kind-inferencing
--- ok
