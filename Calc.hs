{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified StackVM as VM
import Parser

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr VM.Program where
    lit x = [VM.PushI x]
    add x y = x ++ y ++ [VM.Add]
    mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile x = parseExp lit add mul x
