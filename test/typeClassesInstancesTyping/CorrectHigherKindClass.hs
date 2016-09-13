
class Valid m where
  f :: m () -> m Int
  g :: m (Int -> Int) -> m Bool

--- option: --overloading
--- option: --kind-inferencing
--- ok
