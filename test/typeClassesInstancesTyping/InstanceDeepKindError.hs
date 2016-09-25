
-- Tests whether a kind error inside an instance is detected.
-- To be clear, only the code inside the where is at fault.

class Display a where
  display :: a -> String

instance Display () where
  display _ = let x :: Char Int -- kind error
                  x = undefined
              in ""

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (9,24): Illegal type in type application
--- |  type             : Char Int
--- |  type constructor : Char
--- |    kind           : *     
--- |    does not match : * -> *
