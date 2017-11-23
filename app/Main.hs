module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment

type Ball = (Float, Float, Float, Float)

g :: Float
g = 10

bounce :: Float
bounce = 1

main :: IO ()
main = do
    simulate FullScreen black 60 initial draw update
  where
    update _ = next

initial :: Ball
initial = (300, -300, 300, 0)

draw :: Ball -> Picture
draw (x, y, _, _) = translate x y $ color white $ circleSolid 20

next :: Float -> Ball -> Ball
next dt = accelerate . (move dt) . collision

collision :: Ball -> Ball
collision (x, y, dx, dy)
  | y <= -430  = (x, y, dx, -dy * bounce)
  | x <= -700 = (x, y, -dx * bounce, dy)
  | y >= 430 = (x, y, dx, -dy * bounce)
  | x >= 700 = (x, y, -dx * bounce, dy)
  | otherwise = (x, y, dx, dy)

accelerate :: Ball -> Ball
accelerate (x, y, dx, dy) = (x, y, dx, dy - g)

move :: Float -> Ball -> Ball
move dt (x, y, dx, dy) = (x + dx * dt, y + dy * dt, dx, dy)
