
-- m: * -> *
class Pointy m where
  point :: a -> m a

--            * -> *       *
--              |          |
--              V          V
instance Pointy m => Show [m] where
  -- point impl not necessary for error message

--- option: --overloading
--- option: --kind-inferencing
--- option: --type-debug
--- error:
--- | Fixme
