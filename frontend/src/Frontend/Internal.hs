{-# LANGUAGE RankNTypes #-}
module Frontend.Internal where

import           Control.Lens         (Getter, Setter', over, view)
import           Data.Functor.Compose (Compose (Compose))
import           Data.Monoid.Endo     (Endo, mapEndo)
import           Data.Text            (Text, pack, unpack)
import           Reflex               (Dynamic, Event, Reflex, attach, current,
                                       updated)
import           Safe                 (readMay)

fget :: Functor f => Getter a b -> f a -> f b
fget g = fmap (view g)

diffDyn :: Reflex t => (a -> a -> b) -> Dynamic t a -> Event t b
diffDyn f d = uncurry f <$> attach (current d) (updated d)

overEndo :: Setter' a b -> Endo b -> Endo a
overEndo = mapEndo . over

readText :: Read a => Text -> Maybe a
readText = readMay . unpack

showText :: Show a => a -> Text
showText = pack . show

(<<$>>) :: (Functor f) => (a -> g b) -> f a -> Compose f g b
(<<$>>) mkG = Compose . fmap mkG
