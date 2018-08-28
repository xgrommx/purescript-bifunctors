module BicomposeFB where

import Data.Newtype (class Newtype)

newtype BicomposeFB p h f a b c = BicomposeFB (p (f a) (h b c))

derive instance newtypeBicomposeFB :: Newtype (BicomposeFB p h f a b c) _