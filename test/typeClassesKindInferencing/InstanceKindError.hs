
class Pointy m where
  point :: a -> m a

class Showy b where
  showy :: b -> String

-- Pointy :: (* -> *) -> Constraint
-- Showy  ::    *     -> Constraint
instance (Pointy p, Showy p) => Showy [p] where

--- option: --overloading
--- option: --kind-inferencing
--- option: --type-debug
--- error:
--- | Fixme
