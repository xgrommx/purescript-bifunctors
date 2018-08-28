module Bicompose where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Newtype (class Newtype, over)

newtype Bicompose p f g a b c d = Bicompose (p (f a b) (g c d))

derive instance newtypeBicompose :: Newtype (Bicompose p f g a b c d) _

instance functorBicompose :: (Bifunctor p, Functor (g c)) => Functor (Bicompose p f g a b c) where
  map f = over Bicompose (rmap (map f))

instance bifunctorBicompose :: (Bifunctor g, Bifunctor p) => Bifunctor (Bicompose p f g a b) where
  bimap f g = over Bicompose (rmap (bimap f g))