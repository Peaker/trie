{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (lookup)
import Control.DeepSeq(NFData(..))
import Control.Arrow((***), (&&&))
import Control.Monad(forever)
import Control.Exception(evaluate)
import Data.Monoid(Monoid(..))
-- import Data.Set(Set)
-- import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import Wrapper(Wrapper(..), inWrapper2)
import System.Environment(getArgs)
-- import Data.MemoTrie(HasTrie)
-- import MemoSet(memoSet)

newtype Trie k v = Trie { unTrie :: (v, Map k (Trie k v)) }
  deriving (Show, Read, Eq, Ord)
nestedTries :: Trie k v -> Map k (Trie k v)
nestedTries = snd . unTrie
onNestedTries :: (Trie k v -> r) -> Map k (Trie k v) -> [r]
onNestedTries f = map f . Map.elems

instance (NFData k, NFData v) => NFData (Trie k v) where
  rnf (Trie x) = rnf x

instance Wrapper (Trie k v) where
  type Unwrapped (Trie k v) = (v, Map k (Trie k v))
  wrap = Trie
  unwrap = unTrie

mappendMap :: (Ord k, Monoid v) => Map k v -> Map k v -> Map k v
mappendMap = Map.unionWith mappend

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

tupleApply :: (a -> a', b -> b') -> (a, b) -> (a', b')
tupleApply = uncurry (***)

tupleApply2 :: (a -> a' -> a'', b -> b' -> b'') -> (a, b) -> (a', b') -> (a'', b'')
tupleApply2 = (result . result) tupleApply tupleApply

instance (Ord k, Monoid v) => Monoid (Trie k v) where
  mempty = wrap mempty
  mappend = (inWrapper2 . tupleApply2) (mappend, mappendMap)

value :: (Monoid v) => [k] -> v -> Trie k v
value [] v = Trie (v, Map.empty)
value (k:ks) v = Trie (mempty, Map.singleton k $ value ks v)

build :: (Ord k, Monoid v) => [([k], v)] -> Trie k v
build = mconcat . map (uncurry value)

suffixTrie :: (Ord k, Monoid v) => Trie k v -> Trie k v
suffixTrie t = mconcat . (t:) . onNestedTries suffixTrie . nestedTries $ t

prefixTrie :: (Monoid v) => Trie k v -> Trie k v
prefixTrie t = Trie (v', m')
  where
    (v, m) = unTrie t
    v' = mappend v . mconcat $ onNestedTries (fst . unTrie) m'
    m' = Map.map prefixTrie m

substringTrie :: (Ord k, Monoid v) => Trie k v -> Trie k v
substringTrie = prefixTrie . suffixTrie

lookup :: (Ord k, Monoid v) => [k] -> Trie k v -> v
lookup [] = fst . unTrie
lookup (k:ks) = maybe mempty (lookup ks) . Map.lookup k . snd . unTrie

-- newtype MSet a = MSet (Set a)
--   deriving (Eq, Ord, Show, Read)
-- instance NFData a => NFData (MSet a) where
--   rnf (MSet x) = rnf x

-- instance (HasTrie k, Ord k) => Monoid (MSet k) where
--   mempty = MSet mempty
--   MSet x `mappend` MSet y = MSet . memoSet $ x `mappend` y

main :: IO ()
main = do
  n <- fmap (read . head) $ getArgs
  ws <- fmap lines . readFile $ "/usr/share/dict/words"
  print $ length ws
  let t = substringTrie .
          build .
          map (id &&& (:[])) .
          take n $
          ws
  evaluate . rnf $ t
  print "Made trie"
  forever $ do
    line <- getLine
    print line
    print $ lookup line t
