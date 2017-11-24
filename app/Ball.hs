module Ball where

data Ball = Ball
  { x :: Float
  , y :: Float
  , dx :: Float
  , dy :: Float
  , bounce :: Float
  }

moveX :: Float -> Ball -> Ball
moveX s (Ball x y dx dy bounce) = Ball (x + s) y dx dy bounce

moveY :: Float -> Ball -> Ball
moveY s (Ball x y dx dy bounce) = Ball x (y + s) dx dy bounce

accelerateX :: Float -> Ball -> Ball
accelerateX a (Ball x y dx dy bounce) = Ball x y (dx + a) dy bounce

accelerateY :: Float -> Ball -> Ball
accelerateY a (Ball x y dx dy bounce) = Ball x y dx (dy + a) bounce

bounceX :: Ball -> Ball
bounceX (Ball x y dx dy bounce) = Ball x y (-dx * bounce) dy bounce

bounceY :: Ball -> Ball
bounceY (Ball x y dx dy bounce) = Ball x y dx (-dy * bounce) bounce
