module Bicompose1 where

import Data.Newtype (class Newtype)

newtype Bicompose1 p f g a b = Bicompose1 (p (f a b) (g a b))

derive instance newtypeBicompose1 :: Newtype (Bicompose1 p f g a b) _