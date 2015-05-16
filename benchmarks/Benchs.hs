
import           Criterion.Main
import           Data.Traversable
import           System.Random

import           Trie
import           TrieSpec

main :: IO ()
main = defaultMain $
  env (mkSets 1000 200) (\ sets ->
    bench "union" (nf (uncurry union) sets)) :
  env (mkSets 1000 200) (\ sets ->
    bench "naiveUnion" (nf (uncurry naiveUnion) sets)) :
  []

mkSets :: Integer -> Integer -> IO (Trie, Trie)
mkSets n m = (,) <$> inner <*> inner
 where
  inner :: IO Trie
  inner =
    fromList <$> forM [1 .. n] (const $ do
      len <- randomRIO (0, m)
      forM [1 .. len] $ const $ do
        randomRIO ('a', 'z'))
