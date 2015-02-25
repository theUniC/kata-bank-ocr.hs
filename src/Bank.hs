module Bank (
  Account(..),
  parseAccount,
  isValid
)	where

import Data.List (transpose)
import Data.Char (digitToInt)

data Account = AccountNumber [Int]
               deriving(Show, Eq)

parseAccount :: String -> Maybe Account
parseAccount "" = Nothing
parseAccount s  = Just $ AccountNumber $ parseAccount' s

parseAccount' :: String -> [Int]
parseAccount' = foldl f [] . prep
  where
    prep = map concat . transpose . map triples . init . lines
    f account digit = account ++ [parseDigit digit]


triples :: [a] -> [[a]]
triples (a:b:c:xs) = [a, b, c] : triples xs
triples _          = []

parseDigit :: String -> Int
parseDigit " _ | ||_|"  = 0
parseDigit "     |  |"  = 1
parseDigit " _  _||_ "  = 2
parseDigit " _  _| _|"  = 3
parseDigit "   |_|  |"  = 4
parseDigit " _ |_  _|"  = 5
parseDigit " _ |_ |_|"  = 6
parseDigit " _   |  |"  = 7
parseDigit " _ |_||_|"  = 8
parseDigit " _ |_| _|"  = 9
parseDigit _            = error "unknown symbol"

isValid :: Int -> Bool
isValid account = num `mod` 11 == 0
  where
    num = sum [ i * digitToInt d | (i, d) <- zip [1..9] (reverse . show $ account)]
