module Main where

import Data.Bifunctor
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Ball
import Wall

g :: Float
g = 10

main :: IO ()
main = do
    (width, height) <- getScreenSize
    let initialState = initial width height
    simulate window background fps initialState render update
  where
    window = FullScreen
    background = black
    fps = 60
    render = draw
    update _ = next

initial :: Int -> Int -> (Room, Ball)
initial width height = do
  let room = roomFromWidthHeight (fromIntegral width) (fromIntegral height)
  let ball = Ball 300 (-300) 300 0 1

  (room, ball)

draw :: (Room, Ball) -> Picture
draw (_, ball) = translate (x ball) (y ball) $ color white $ circleSolid 20

next :: Float -> (Room, Ball) -> (Room, Ball)
next dt = second (accelerateY (-g) . move dt) . collision

collision :: (Room, Ball) -> (Room, Ball)
collision (room, ball)
  | y ball <= roomFloor room + 20 = (room, bounceY ball)
  | x ball <= roomLeft room + 20 = (room, bounceX ball)
  | y ball >= roomCeiling room - 20 = (room, bounceY ball)
  | x ball >= roomRight room - 20 = (room, bounceX ball)
  | otherwise = (room, ball)

move :: Float -> Ball -> Ball
move dt ball = (moveX $ dx ball * dt) $ (moveY $ dy ball * dt) ball
