module Ball where

data Ball = Ball
  { x :: Float
  , y :: Float
  , dx :: Float
  , dy :: Float
  }

bounce :: Float
bounce = 1

moveX :: Float -> Ball -> Ball
moveX s (Ball x y dx dy) = Ball (x + s) y dx dy

moveY :: Float -> Ball -> Ball
moveY s (Ball x y dx dy) = Ball x (y + s) dx dy

accelerateX :: Float -> Ball -> Ball
accelerateX a (Ball x y dx dy) = Ball x y (dx + a) dy

accelerateY :: Float -> Ball -> Ball
accelerateY a (Ball x y dx dy) = Ball x y dx (dy + a)

bounceX :: Ball -> Ball
bounceX (Ball x y dx dy) = Ball x y (-dx * bounce) dy

bounceY :: Ball -> Ball
bounceY (Ball x y dx dy) = Ball x y dx (-dy * bounce)
