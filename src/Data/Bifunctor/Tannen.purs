module Tannen where

import Prelude

import Control.Apply (lift2)
import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, (<<*>>))
import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Newtype (class Newtype, over)

newtype Tannen f p a b = Tannen (f (p a b))

derive instance newtypeTannen :: Newtype (Tannen f p a b) _

derive newtype instance eqTannen :: Eq (f (p a b)) => Eq (Tannen f p a b)

derive newtype instance ordTannen :: Ord (f (p a b)) => Ord (Tannen f p a b)

instance showTannen :: Show (f (p a b)) => Show (Tannen f p a b) where
  show (Tannen x) = "(Tannen " <> show x <> ")"

instance functorTannen :: (Functor f, Bifunctor p) => Functor (Tannen f p a) where
  map f = over Tannen (map (rmap f))

instance bifunctorTannen :: (Functor f, Bifunctor p) => Bifunctor (Tannen f p) where
  bimap f g = over Tannen (map (bimap f g))

instance biapplyTannen :: (Apply f, Biapply p) => Biapply (Tannen f p) where
  biapply (Tannen fg) (Tannen xy) = Tannen (lift2 (<<*>>) fg xy)

instance biapplicativeTannen :: (Applicative f, Biapplicative p) => Biapplicative (Tannen f p) where
  bipure a b = Tannen (pure (bipure a b))