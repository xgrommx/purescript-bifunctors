module BicomposeBF where

import Data.Newtype (class Newtype)

newtype BicomposeBF p h f a b c = BicomposeBF (p (h a b) (f c))

derive instance newtypeBicomposeBF :: Newtype (BicomposeBF p h f a b c) _