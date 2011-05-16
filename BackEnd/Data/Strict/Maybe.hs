module Data.Strict.Maybe (Maybe(..),
                          maybe,
                          isJust,
                          isNothing,
                          fromJust,
                          fromMaybe,
                          listToMaybe,
                          maybeToList,
                          catMaybes,
                          mapMaybe)
  where

import Control.DeepSeq
import Prelude hiding (Maybe(..), maybe)


data Maybe a = Nothing
             | Just !a
             deriving (Eq, Show)


instance (NFData a) => NFData (Maybe a) where
  rnf Nothing = ()
  rnf (Just something) = rnf something


instance Monad Maybe where
  (>>=) Nothing actionB = Nothing
  (>>=) (Just resultA) actionB = actionB resultA
  return a = Just a


maybe :: b -> (a -> b) -> Maybe a -> b
maybe default' _ Nothing = default'
maybe _ function (Just a) = function a


isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True


isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False


fromJust :: Maybe a -> a
fromJust Nothing = error $ "fromJust: Nothing"
fromJust (Just a) = a


fromMaybe :: a -> Maybe a -> a
fromMaybe default' Nothing = default'
fromMaybe _ (Just a) = a


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]


catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:rest) = catMaybes rest
catMaybes (Just a:rest) = a : catMaybes rest


mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe function input = catMaybes $ map function input
