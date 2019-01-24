{-# LANGUAGE RankNTypes            #-}
module Frontend.Internal where

import Control.Lens (Getter, view)

fget :: Functor f => Getter a b -> f a -> f b
fget g = fmap (view g)
