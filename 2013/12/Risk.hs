{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (replicateM)
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

n_wins :: [(Int, Int)] -> (Int, Int)
n_wins list = (atk_wins list, def_wins list)
  where def_wins = sum . map (\(x,y) -> if x > y then 0 else 1)
        atk_wins = sum . map (\(x,y) -> if x > y then 1 else 0)

sortDescending :: Ord a => [a] -> [a]
sortDescending = reverse . sort

{-- Exercise 2 --}
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  let n_attackers = (attackers bf) - 1
  let n_defenders = (defenders bf) 
  attack_dies <- replicateM n_attackers die
  defence_dies <- replicateM n_defenders die
  let (atk_wins, def_wins) = n_wins $ zip (sortDescending (unDV <$> attack_dies)) 
                                          (sortDescending (unDV <$> defence_dies))
  return $ Battlefield (attackers bf - def_wins) (defenders bf - atk_wins)

{-- Exercise 3 --}
invade :: Battlefield -> Rand StdGen Battlefield
invade bf 
  | defenders bf == 0 = return bf
  | attackers bf < 2 = return bf
  | otherwise = do
    new_bf <- battle bf
    invade new_bf

{- Exercise 4 --}
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  battlefields <- replicateM 1000 (invade bf)
  let wins = sum $ map (\x -> if attackers x > defenders x then 1 else 0) battlefields
  return (wins / 1000)
    
  

