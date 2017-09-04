module Freq where

import Data.List
import Data.Hashable
import Data.HashMap (Map)
import qualified Data.HashMap as Map
import Data.Ord


k_mers :: Int -> [a] -> [[a]]
k_mers k w =
    take (length w - k + 1) . map (take k) $ tails w

freq_map :: (Hashable a, Ord a) => Map a Int -> [a] -> Map a Int
freq_map initialMap l = foldr (Map.alter add) initialMap l
  where
    add Nothing = Just 1
    add (Just x) = Just $ x + 1

top :: [(a, Int)] -> ([a], Int)
top l = (map fst xcs, snd $ head xcs)
  where
    xcs = takeWhile (\x -> snd x == snd (head l)) l

most_frequent :: (Hashable a, Ord a) => [a] -> ([a], Int)
most_frequent l = top . reverse $ sortBy (comparing snd) freq
  where
    freq = Map.toList $ freq_map Map.empty l

unDown :: Down a -> a
unDown (Down x) = x
