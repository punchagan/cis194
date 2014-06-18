{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import           Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
    fmap f (Parser {runParser=g}) = Parser {runParser=h}
        where h x = fmap (first f) (g x)

-- Parser (a -> b) -> Parser a -> Parser b
-- Parser (Char -> Int) -> Parser Char -> Parser Int

instance Applicative Parser where
    pure x = Parser {runParser=(\ s -> Just(x, s))}
    Parser {runParser=f} <*> Parser {runParser=g} = Parser {runParser=h}
        where
            h xs
                | isNothing $ f xs = Nothing
                | isNothing $ g xs' = Nothing
                | otherwise = Just (f' g', xs'')
                where Just (f', xs') = (f xs)
                      Just (g', xs'') = (g xs')


parseName = Parser $ p where
    p xs = Just (takeWhile (`elem` alpha) xs, dropWhile (`elem` alpha) xs)
        where alpha = ['A'..'Z'] ++ ['a'..'z'] ++ [' ', '.']

parsePhone = Parser $ q where
    q xs = Just (takeWhile (`elem` num) xs, dropWhile (`elem` num) xs)
        where num = ['0'..'9'] ++ ['-']

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving (Show)

-- runParser (Emp <$> parseName <*> parsePhone) "Puneeth Chaganti929-250-8115"

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (const . const ()) <$> ((const ()) <$> char 'a') <*> ((const ()) <$> char 'b')

intPair :: Parser [Integer]
intPair = (\ a _ b -> [a, b]) <$> posInt <*> char ' ' <*>  posInt

instance Alternative Parser where
    empty = Parser h where h = \ _ -> Nothing
    Parser f <|> Parser g = Parser h
        where h xs = f xs <|> g xs

intOrUppercase :: Parser ()
intOrUppercase = ((const ()) <$> posInt) <|> ((const ()) <$> satisfy (`elem` ['A'..'Z']))
