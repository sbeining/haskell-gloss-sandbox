module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Ball

g :: Float
g = 10

main :: IO ()
main = do
    simulate window background fps initial render update
  where
    window = FullScreen
    background = black
    fps = 60
    render = draw
    update _ = next

initial :: Ball
initial = Ball 300 (-300) 300 0 1

draw :: Ball -> Picture
draw ball = translate (x ball) (y ball) $ color white $ circleSolid 20

next :: Float -> Ball -> Ball
next dt = accelerateY (-g) . (move dt) . collision

collision :: Ball -> Ball
collision ball
  | y ball <= -430  = bounceY ball
  | x ball <= -700 = bounceX ball
  | y ball >= 430 = bounceY ball
  | x ball >= 700 = bounceX ball
  | otherwise = ball


move :: Float -> Ball -> Ball
move dt ball = (moveX (dx ball * dt)) (moveY (dy ball * dt) ball)
