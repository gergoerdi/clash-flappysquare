{-# LANGUAGE RecordWildCards #-}
module FlappySquare.Game where

import Clash.Prelude

type ScreenWidth = 640
type ScreenHeight = 480

data St = MkSt
    { wallsTop, wallsBottom :: Vec 10 (Index 480)
    , wallOffset :: Index 640
    , birdSpeed :: Signed 10
    , birdY :: Signed 10
    , gameOver :: Bool
    }
    deriving (Show, Generic, NFDataX)

birdX :: Index ScreenWidth
birdX = 100

initState :: St
initState = MkSt
    { wallOffset = 0
    , birdSpeed = 0
    , birdY = 200
    , gameOver = False
    , ..
    }
  where
    (wallsTop, wallsBottom) = unzip $
          (130, 290) :>
          (80, 280) :>
          (60, 270) :>
          (90, 320) :>
          (110, 430) :>
          (200, 410) :>
          (230, 400) :>
          (130, 380) :>
          (90, 320) :>
          (110, 280) :>
          Nil

updateState :: Bool -> St -> St
updateState btn s@MkSt{..}
  | gameOver = initState
  | otherwise = s
    { wallOffset = satAdd SatWrap 1 wallOffset
    , birdSpeed = birdSpeed + if btn then -15 else 3
    , birdY = birdY + birdSpeed `shiftR` 3
    , gameOver =
          (birdY - 20) < fromIntegral (wallsTop !! idx) ||
          (birdY + 20) > fromIntegral (wallsBottom !! idx)
    }
  where
    (idx, offset) = wallAt birdX s

wallAt :: Index ScreenWidth -> St -> (Index 10, Index 64)
wallAt x MkSt{..} = bitCoerce $ satAdd SatWrap x wallOffset

x `between` (lo, hi) = lo <= x && x <= hi
