{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Character.Classes.Spec where

import Character (Character)
import           Data.Equivalence.Class (HasEP (..))
import           Dist                   (Dist)
import           Spells.Spell           (Spell)

-- | Spec represents the inputs required to run dps calculations
data Spec a where
  Spec :: (Show a, HasEP a) =>
    { inputs :: [a]
    , mkSpells  :: Character -> Dist (Spell Character)
    , buffScale :: Float -> Float
    } -> Spec a

-- | Oh, how we must circumvent Functor instances with Constraints.
-- I wonder if there's a better way?
newtype FSpec a =
  FSpec
    { unwrap :: forall b. (Show b, HasEP b) => (a -> b) -> Spec b
    }

instance Functor FSpec where
  -- x :: (a -> b) -> Spec b
  fmap f (FSpec x) = FSpec (\f' -> x (f' . f))

-- | liftW lifts a Spec into a FSpec which impls Functor
liftW :: Spec a -> FSpec a
liftW s = FSpec $ \f ->
  Spec
    { inputs = map f (inputs s)
    , mkSpells = mkSpells s
    , buffScale = buffScale s
    }

-- | lowerW lowers a FSpec into a Spec
lowerW :: (Show a, HasEP a) => FSpec a -> Spec a
lowerW w = unwrap w id

-- | sMap is syntactic sugar for fmap'ing over a Spec lifted into a FSpec
sMap :: (Show b, HasEP b) => (a -> b) -> Spec a -> Spec b
sMap f = lowerW . (fmap f) . liftW
