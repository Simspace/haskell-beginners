{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

{- Set 1: Random Numbers -}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands =
  let (n1, s2) = rand (mkSeed 1)
      (n2, s3) = rand s2
      (n3, s4) = rand s3
      (n4, s5) = rand s4
      (n5, _) = rand s5
   in [n1, n2, n3, n4, n5]

nRands :: Seed -> Int -> [Integer]
nRands _ 0 = []
nRands seed n =
  let (n', seed') = rand seed
   in n' : nRands seed' (n - 1)

-- >>> product fiveRands
-- 8681089573064486461641871805074254223660

-- >>> product (nRands (mkSeed 1) 5)
-- 8681089573064486461641871805074254223660

randString3 :: String
randString3 =
  let (n1, s2) = rand (mkSeed 1)
      (n2, s3) = rand s2
      (n3, _) = rand s3
   in map toLetter [n1, n2, n3]

-- >>> randString3
-- "lrf"

-- A helpful "type alias" for generators
type Gen a = Seed -> (a, Seed)

-- Seed -> (Integer, Seed)
randEven :: Gen Integer -- the output of rand * 2
randEven s =
  let (n1, s1) = rand s
   in (n1 * 2, s1)

-- >>> :t (* 2)
-- (* (2 :: Int)) :: Int -> Int

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd s =
  let (n1, s1) = randEven s
   in (n1 + 1, s1)

-- >>> randTen 5
randTen :: Gen Integer -- the output of rand * 10
randTen s =
  let (n1, s1) = randEven s
   in (n1 * 5, s1)

randLetter :: Gen Char
randLetter seed =
  let (n, seed') = rand seed
   in (toLetter n, seed')

-- generalA :: (a -> b) -> Gen a -> Seed -> (b, Seed)
generalA :: (a -> b) -> Gen a -> Gen b
generalA f randA s =
  let (n1, s1) = randA s
   in (f n1, s1)

-- 👆 This is `fmap` (or Functor's `map`) for `Gen`!

randPair :: Gen (Char, Integer)
randPair = \seed ->
  let (c, seed') = randLetter seed
      (n, seed'') = rand seed'
   in ((c, n), seed'')

-- >>> randPair (mkSeed 1)
-- (('l',282475249),Seed {unSeed = 282475249})

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair genA genB seed =
  let (a, seed') = genA seed
      (b, seed'') = genB seed'
   in ((a, b), seed'')

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f genA genB = \seed ->
  let (a, seed') = genA seed
      (b, seed'') = genB seed'
   in (f a b, seed'')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

repRandom :: [Gen a] -> Gen [a]
-- repRandom :: [Seed -> (a, Seed)] -> Seed -> ([a], Seed)
repRandom [] = ([],)
repRandom (genA : genAs) = \seed ->
  let (a, seed') = genA seed
      (as, seed'') = repRandom genAs seed'
   in (a : as, seed'')

repRandom' :: [Gen a] -> Gen [a]
repRandom' = foldl (flip $ generalB (:)) ([],)
-- repRandom' [] = ([],)
-- repRandom' (genA : genAs) =
--   generalB (:) genA (repRandom' genAs)

-- >>> repRandom (replicate 3 randLetter) (mkSeed 1)
-- ("lrf",Seed {unSeed = 1622650073})
-- >>> repRandom' (replicate 3 randLetter) (mkSeed 1)
-- ("lrf",Seed {unSeed = 1622650073})

-- >>> :t (:)
-- (:) :: forall a. a -> [a] -> [a]
