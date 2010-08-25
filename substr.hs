{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}

import Control.DeepSeq(rnf)
import Control.Monad(forever)
import qualified Data.List as L
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Exception(evaluate)
import MemoSet(memoSet)
import Data.MemoTrie(HasTrie(..))

substrings :: [a] -> [[a]]
substrings = concatMap L.inits . L.tails

build :: (HasTrie k, Ord k) => [[k]] -> Map [k] (Set [k])
build ws = Map.fromListWith ((memoSet .) . Set.union) $ l
  where
    l = [ (substr, (memoSet . Set.singleton) word)
        | word <- ws
        , substr <- substrings word ]

main :: IO ()
main = do
  ws <- fmap (take 10000 . lines) . readFile $ "/usr/share/dict/words"
  print $ length ws
  let t = build ws
  evaluate . rnf $ t
  print $ Map.size t
  forever $ do
    print "Give me a line:"
    line <- getLine
    print $ Map.lookup line t
