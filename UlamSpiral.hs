{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Monoid (Sum (..))
import Data.Ord (comparing)
import Data.Vector ((!), maximumBy)
import Math.NumberTheory.ArithmeticFunctions (bigOmegaA, smallOmegaA)
import Math.NumberTheory.ArithmeticFunctions.SieveBlock (runFunctionOverBlock)
import Options.Applicative

import qualified System.Console.Terminal.Size as TS

newtype Options = Options
  { optSize :: Int
  }

main :: IO ()
main = do

  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <- customExecParser
    ( prefs $ columns cols )
    ( info
      ( do
          optSize <- option auto $
            short 'z' <> long "size" <> metavar "INT" <> value 19 <>
            help "Size of the spiral (width or height)" <> showDefault
          pure Options {..}
      )
      ( fullDesc <> header "Draw an Ulam Spiral as an SVG file" )
    )

  print $ ulamSpiral optSize

ulamSpiral :: Int -> (Sum Word, Word)
ulamSpiral w =
  (foldMap ulamPoint [(x, y) | x <- [-r .. r], y <- [-r .. r]], nColours)
 where
  r = w `div` 2
  nColours = fst $ maximumBy (comparing fst) uniqueFactorCounts
  uniqueFactorCounts = runFunctionOverBlock @(Word, Word)
    (liftA2 (,) smallOmegaA bigOmegaA) 2 (fromIntegral $ w * w)
  uniqueFactorCount n
    | n >= 2 = case uniqueFactorCounts ! (n - 2) of
        (_, 1) -> 0
        (c, _) -> c
    | otherwise = 1
  ulamPoint (x, y) = Sum c where !c = uniqueFactorCount $ ulamNumber (x, y)

ulamNumber :: (Int, Int) -> Int
ulamNumber (x, y) = midRightIndex + midRightOffset
  where
    r = max (abs x) (abs y)
    d = 2 * r
    innerArea = (d - 1) ^ (2 :: Int)
    midRightIndex = innerArea + r
    midRightOffset
      | abs x > abs y = if x > 0 then 0 * d + y else 2 * d - y
      | otherwise     = if y > 0 then 1 * d - x else 3 * d + x
