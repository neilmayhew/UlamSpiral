{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSV (hsv)
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Diagrams.Prelude hiding (font)
import Graphics.SVGFonts.PathInRect (PathInRect (..), fit_height)
import Graphics.SVGFonts.ReadFont (PreparedFont, loadFont)
import Graphics.SVGFonts.Text (TextOpts, svgText, textFont)

main :: IO ()
main = do
  font <- loadFont "fonts/Roboto-Medium.svg"
  mainWith $
    bgFrame 0.1 darkslategrey
      . ulamSpiral font

ulamSpiral :: PreparedFont (N B) -> Int -> Diagram B
ulamSpiral font w =
  foldMap
    (ulamCircle font)
    [(x, y) | x <- [-r .. r], y <- [-r .. r]]
 where
  r = w `div` 2

ulamCircle :: PreparedFont (N B) -> (Int, Int) -> Diagram B
ulamCircle font (x, y) =
  let r = max (abs x) (abs y)
      d = 2 * r
      innerArea = (d - 1) ^ (2 :: Int)
      midRightIndex = innerArea + r
      midRightOffset
        | abs x > abs y = if x > 0 then 0 * d + y else 2 * d - y
        | otherwise     = if y > 0 then 1 * d - x else 3 * d + x
      number = midRightIndex + midRightOffset
      colour i = uncurryRGB sRGB $ hsv (180 + realToFrac (i - 1) * 36) 0.35 0.9
   in translate (realToFrac <$> x ^& y) $
        mconcat
          [ label font (show number) # fc white
          , circle 0.5 # lw none # fc (colour (-r))
          ]

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
