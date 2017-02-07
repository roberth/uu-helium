import TestClasses
--      (*->*)->C,  *
instance Wildcard Int where

--- option: --overloading
--- option: --kind-inferencing
--- error:
--- | (3,1): Illegal type in instance declaration
--- |  the class        : Wildcard
--- |  has type         : (* -> *) -> Constraint
--- |  but used as if   : *        -> Constraint
