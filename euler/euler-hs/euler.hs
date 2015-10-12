import System.Environment
import Data.List

isFactorOf :: Int -> Int -> Bool
isFactorOf x y = x `mod` y == 0 

solution1 :: String
solution1 = show ( sum [x | x <- [1..999], x `isFactorOf` 3 || x `isFactorOf` 5] )

solutions :: [String]
solutions = [solution1]

-- | 'main' runs the main program
main :: IO()
main = print( zip [1..] solutions )
