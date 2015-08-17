{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> Filename -> IO (List Chars)
fastAnagrams word fPath = validWords . lines <$> readFile fPath
  where
    isValidWord :: List Chars -> Chars -> Bool
    isValidWord = flip S.member . S.fromList . hlist
    validWords dict = filter (isValidWord dict) (permutations word)

newtype NoCaseString = NoCaseString
                       { ncString :: Chars }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
