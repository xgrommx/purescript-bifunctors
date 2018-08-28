module Mu where

import Prelude

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, bilift2)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Newtype (class Newtype, over)

newtype Mu p a = In (p (Mu p a) a)

derive instance newtypeMu :: Newtype (Mu p a) _

derive instance eqMu :: Eq (p (Mu p a) a) => Eq (Mu p a)
derive instance ordMu :: Ord (p (Mu p a) a) => Ord (Mu p a)

instance showMu :: Show (p (Mu p a) a) => Show (Mu p a) where
  show (In x) = "(In " <> show x <> ")" 

instance functorMu :: Bifunctor p => Functor (Mu p) where
  map f = over In (bimap (map f) f)

instance applyMu :: Biapply p => Apply (Mu p) where
  apply (In p) (In q) = In (bilift2 (<*>) ($) p q)

instance applicativeMu :: Biapplicative p => Applicative (Mu p) where
  pure a = In (bipure (pure a) a)