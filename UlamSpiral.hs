{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Ord (comparing)
import Data.Vector ((!), maximumBy)
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Diagrams.Prelude hiding (font)
import Graphics.SVGFonts.PathInRect (PathInRect (..), fit_height)
import Graphics.SVGFonts.ReadFont (PreparedFont, loadFont)
import Graphics.SVGFonts.Text (TextOpts, svgText, textFont)
import Math.NumberTheory.ArithmeticFunctions (bigOmegaA, smallOmegaA)
import Math.NumberTheory.ArithmeticFunctions.SieveBlock (runFunctionOverBlock)

main :: IO ()
main = do
  font <- loadFont "fonts/Roboto-Medium.svg"
  mainWith $
    bgFrame 0.1 darkslategrey
      . ulamSpiral font

ulamSpiral :: PreparedFont (N B) -> Int -> Diagram B
ulamSpiral font w =
  foldMap ulamPoint [(x, y) | x <- [-r .. r], y <- [-r .. r]]
 where
  r = w `div` 2
  colour 0 = uncurryRGB sRGB $ hsv 0 1 1
  colour i = uncurryRGB sRGB $ hsv (300 - realToFrac i * 360 / realToFrac nColours) 0.875 0.375
  nColours = fst $ maximumBy (comparing fst) uniqueFactorCounts
  uniqueFactorCounts = runFunctionOverBlock @(Word, Word)
    (liftA2 (,) smallOmegaA bigOmegaA) 2 (fromIntegral $ w * w)
  uniqueFactorCount n
    | n >= 2 = case uniqueFactorCounts ! (n - 2) of
        (_, 1) -> 0
        (c, _) -> c
    | otherwise = 1
  ulamPoint (x, y) = translate (realToFrac <$> x ^& y) $
    if w <= 31
      then ulamCircle font n c
      else ulamSquare c
    where
      n = ulamNumber (x, y)
      c = colour $ uniqueFactorCount n

ulamCircle :: PreparedFont (N B) -> Int -> Colour (N B) -> Diagram B
ulamCircle font number colour =
  mconcat
    [ label font (show number) # fc white
    , circle 0.5 # lw none # fc colour
    ]

ulamSquare :: Colour (N B) -> Diagram B
ulamSquare colour = rect 1 1 # lw none # fc colour

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

label :: PreparedFont (N B) -> String -> Diagram B
label font s = toText s # center
 where
  toText = lw none . set_envelope . fit_height 0.5 . svgText defOpts { textFont = font }
  defOpts = def :: TextOpts (N B)

  set_envelope ::
    forall b n.
    (TypeableFloat n, Renderable (Path V2 n) b) =>
    PathInRect n ->
    QDiagram b V2 n Any
  set_envelope (PathInRect _x1 _y1 _x2 _y2 path) =
    path # stroke
