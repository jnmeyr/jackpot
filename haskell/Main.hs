module Main (main) where

import Foreign
import Data.Char (digitToInt)

vonNeumann :: Integer -> Int -> Integer
vonNeumann seed power = seed ^ 2 `div` (10 ^ (power `div` 2)) `mod` (10 ^ power)

readHex :: String -> Integer
readHex ('0' : 'x' : string) = readHex' string ((length string) - 1)
  where
    readHex' :: String -> Int -> Integer
    readHex' "" _ = 0
    readHex' (char : string) power = toInteger ((16 ^ power) * digitToInt char) + readHex' string (power - 1)

addressValue :: ForeignPtr a -> Integer
addressValue = readHex . show

reduceValue :: Integer -> Integer -> Integer -> Integer
reduceValue minValue maxValue value = value `mod` (maxValue - minValue + 1) + minValue

generate :: Integer -> Integer -> Integer -> IO [Integer]
generate _ _ 0 = return []
generate minValue maxValue number = do
  ptr <- mallocForeignPtr :: IO (ForeignPtr Bool)
  let value = reduceValue minValue maxValue $ vonNeumann (addressValue ptr) 8
  values <- generate minValue maxValue (number - 1)
  return (value : values)

main :: IO ()
main = do
  generate 1 50 5 >>= print
  generate 1 10 2 >>= print

