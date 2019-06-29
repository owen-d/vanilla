module Data.Equivalence.Class where

import           Character.Sheet (Character)

class HasEP a where
  combine :: a -> Character -> Character
