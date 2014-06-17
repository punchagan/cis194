module Party where

import Data.List (intercalate, sort)
import Data.Monoid
import Data.Tree
import Employee
import System.IO

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs f) = GL (x:xs) (empFun x + f)

instance Monoid GuestList where
    mempty = (GL [] 0)
    mappend (GL xs fx) (GL ys fy) = (GL (xs ++ ys) (fx + fy))

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e glPairs =  (withNode, withoutNode) where
    withNode = glCons e (mconcat $ map snd glPairs)
    withoutNode = mconcat $ (map (uncurry moreFun) glPairs)

-- we already import this from Data.Tree
-- data Tree a = Node {
--     rootLabel :: a, -- label value
--     subForest :: [Tree a] -- zero or more child trees
-- }

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node {rootLabel=l, subForest=sf}) = f l (map (treeFold f) sf)

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun (treeFold nextLevel tree)

showGL :: GuestList -> IO ()
showGL (GL xs f) = do
    putStrLn $ "Total fun: " ++ show f
    putStrLn $ intercalate "\n" $ sort $ map empName xs

main :: IO ()
main = do
    contents <- readFile "company.txt"
    showGL $ maxFun (read contents :: Tree Employee)
