module ListZipper where

import Prelude hiding (length)
import Prelude as P

data ListZipper a = ListZipper [a] a [a]
                  deriving (Eq, Show)

lefts :: ListZipper a -> [a]
lefts (ListZipper l _ _) = l

rights :: ListZipper a -> [a]
rights (ListZipper _ _ r) = r

toList :: ListZipper a -> [a]
toList (ListZipper l c r) = l ++ [c] ++ r

length :: ListZipper a -> Int
length (ListZipper l _ r) = P.length l + P.length r + 1

focus :: ListZipper a -> a
focus (ListZipper _ c _) = c

sameAroundFocus :: Eq a => ListZipper a -> ListZipper a
sameAroundFocus (ListZipper l c r) =
  ListZipper (f l) c (f r)
  where f = takeWhile (== c)

takeLeft :: Int -> ListZipper a -> ListZipper a
takeLeft n (ListZipper l c r) = ListZipper (take n l) c r

takeRight :: Int -> ListZipper a -> ListZipper a
takeRight n (ListZipper l c r) = ListZipper l c (take n r)

instance Functor ListZipper where
  fmap f (ListZipper l c r) =
    ListZipper (fmap f l) (f c) (fmap f r)
