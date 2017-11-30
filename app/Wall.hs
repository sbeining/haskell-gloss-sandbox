module Wall where

data Wall = Wall
  { from :: (Float, Float)
  , to :: (Float, Float)
  } deriving Show

data Room = Room
  { top :: Wall
  , right :: Wall
  , bottom :: Wall
  , left :: Wall
  } deriving Show

roomFromWidthHeight :: Float -> Float -> Room
roomFromWidthHeight width height = do
  let left = (-width) / 2
  let right = width / 2
  let top = height / 2
  let bottom = (-height) / 2

  Room
    { top = Wall (left, top) (right, top)
    , right = Wall (right, top) (right, bottom)
    , bottom = Wall (left, bottom) (right, bottom)
    , left = Wall (left, top) (left, bottom)
    }

roomFloor :: Room -> Float
roomFloor room = snd (from (bottom room))

roomCeiling :: Room -> Float
roomCeiling room = snd (from (top room))

roomLeft :: Room -> Float
roomLeft room = fst (from (left room))

roomRight :: Room -> Float
roomRight room = fst (from (right room))

