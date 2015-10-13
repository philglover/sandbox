import System.Environment
import Data.List

isFactorOf :: Int -> Int -> Bool
isFactorOf x y = x `mod` y == 0 

fibonacci :: (Int -> Bool) -> [Int]
fibonacci p = fibloop 1 2 p
fibloop :: Int -> Int -> (Int -> Bool) -> [Int]
fibloop a b p
  | p(a + b) = [a, b]
  | otherwise = [a] ++ fibloop b (a + b) p

solution1 :: String
solution1 = show ( sum [x | x <- [1..999], x `isFactorOf` 3 || x `isFactorOf` 5] )

solution2 :: String
solution2Limit = \x -> x >= 4000000
solution2 = show ( sum [ x | x <- fibonacci solution2Limit, x `isFactorOf` 2] )

solutions :: [String]
solutions = [solution1, solution2]

-- | 'main' runs the main program
main :: IO()
main = print( zip [1..] solutions )
