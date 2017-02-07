
-- Prefer not to change this file, so it can be used in many places.

module TestClasses where

class Default a where
  def :: a

class Wildcard m where
  wildcard :: m a

