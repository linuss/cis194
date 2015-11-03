{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char
import Data.Map.Strict 

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score a) = a

points :: Map Char Int
points = fromList $ zip ['a'..'z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
score :: Char -> Score 
score x 
  | member (toLower x) points = Score $ points ! (toLower x)
  | otherwise = 0

scoreString :: String -> Score
scoreString = mconcat . fmap score

