{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}

module MemoSet(memoSet) where

import Control.Arrow(first)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.MemoTrie((:->:), HasTrie(..), memo)

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap . first) f . enumerate

instance (HasTrie k, Ord k) => HasTrie (Set k) where
  data Set k :->: a = SetTrie ([k] :->: a)
  trie f = SetTrie $ trie (f . Set.fromList)
  untrie (SetTrie t) = untrie t . Set.toList
  enumerate (SetTrie t) = enum' Set.fromList t

memoSet :: (Ord k, HasTrie k) => Set k -> Set k
memoSet = memo id
