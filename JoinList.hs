{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (mconcat [tag x, tag y]) x y

-- Safe list indexing
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

-- join list to list
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- index a JoinList
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n (Single m a)
    | n == 0 = Just a
    | otherwise = Nothing
indexJ n (Append m left right)
    | n + 1 > getSize (size m) = Nothing
    | n + 1 > l = indexJ (n - l) right
    | otherwise = indexJ n left
    where l = getSize . size . tag $ left
indexJ _ Empty = Nothing

-- drop the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append m left right)
    | n >= getSize (size m) = Empty
    | n >= l = dropJ (n - l) right
    | otherwise = (dropJ n left) +++ right
    where l = getSize . size . tag $ left
dropJ n x@(Single m _)
    | n > getSize (size m) = Empty
    | otherwise = x

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n x@(Append m left right)
    | n >= getSize (size m) = x
    | n > l = left +++ takeJ (n - l) right
    | otherwise = takeJ n left
    where l = getSize . size . tag $ left
takeJ n x@(Single m _)
    | n >= getSize (size m) = x
    | otherwise = Empty
takeJ _ Empty = Empty

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- Buffer instance
instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ x) = x

  fromString x = (Single (scoreString x, Size 1) x)

  -- -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- -- for out-of-bounds indices.
  line n x = indexJ n x

  -- -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  -- --   with the @n@th line replaced by @ln@.  If the index is
  -- --   out-of-bounds, the buffer should be returned unmodified.
  -- replaceLine :: Int -> String -> b -> b
  replaceLine n x ys
    | n > (getSize . size . tag $ ys) = ys
    | otherwise = (takeJ (n-1) ys) +++ fromString x +++ (dropJ n ys)

  -- -- | Compute the number of lines in the buffer.
  numLines = getSize . snd . tag

  -- -- | Compute the value of the buffer, i.e. the amount someone would
  -- --   be paid for publishing the contents of the buffer.
  value = getScore . fst . tag

main = runEditor editor $ foldr (+++) Empty (map (\ x -> fromString x :: (JoinList (Score, Size) String))
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])
