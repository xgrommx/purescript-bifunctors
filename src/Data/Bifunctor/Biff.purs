module Biff where

import Prelude

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, (<<*>>))
import Data.Bifunctor (class Bifunctor, rmap, bimap)
import Data.Newtype (class Newtype, over)

newtype Biff p f g a b = Biff (p (f a) (g b))

derive instance newtypeBiff :: Newtype (Biff p f g a b) _

derive newtype instance eqBiff :: Eq (p (f a) (g b)) => Eq (Biff p f g a b)

derive newtype instance ordBiff :: Ord (p (f a) (g b)) => Ord (Biff p f g a b)

instance showBiff :: Show (p (f a) (g b)) => Show (Biff p f g a b) where
  show (Biff x) = "(Biff " <> show x <> ")"

instance mapBiff :: (Bifunctor p, Functor g) => Functor (Biff p f g a) where
  map f = over Biff (rmap (map f))

instance bifunctorBiff :: (Bifunctor p, Functor f, Functor g) => Bifunctor (Biff p f g) where
  bimap f g = over Biff (bimap (map f) (map g))

instance biapplyBiff :: (Biapply p, Apply f, Apply g) => Biapply (Biff p f g) where
  biapply (Biff fg) (Biff xy) = Biff (bimap (<*>) (<*>) fg <<*>> xy)

instance biapplicativeBiff :: (Biapplicative p, Applicative f, Applicative g) => Biapplicative (Biff p f g) where
  bipure a b = Biff (bipure (pure a) (pure b))
