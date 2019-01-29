{-# LANGUAGE RankNTypes            #-}
module Frontend.Internal where

import Control.Lens (Getter, view)
import Reflex (Reflex, Dynamic, Event, attach, current, updated)

fget :: Functor f => Getter a b -> f a -> f b
fget g = fmap (view g)

diffDyn :: Reflex t => (a -> a -> b) -> Dynamic t a -> Event t b
diffDyn f d = uncurry f <$> attach (current d) (updated d)
