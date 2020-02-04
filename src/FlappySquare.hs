{-# LANGUAGE NumericUnderscores, RecordWildCards, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module FlappySquare where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.Clock
import Data.Maybe

import FlappySquare.VGA
import FlappySquare.Game
import FlappySquare.Video

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

{-# NOINLINE topEntity #-}
topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "BTN_UP"    ::: Signal Dom25 (Active High)
    -> "BTN_DOWN"  ::: Signal Dom25 (Active High)
    -> "VGA"       ::: VGAOut Dom25
topEntity = withEnableGen board
  where
    board (fmap fromActive -> up) (fmap fromActive -> down) = vgaOut vgaSync rgb
      where
        VGADriver{..} = vgaDriver
        frameEnd = isFalling False (isJust <$> vgaY)

        params = defaultParams
        inputs = MkInputs <$> up <*> down

        st = regEn initState frameEnd $ (updateState params <$> inputs <*> st)

        rgb = draw' params <$> st <*> vgaX <*> vgaY

draw' :: Params -> St -> Maybe (Index ScreenWidth) -> Maybe (Index ScreenHeight) -> Color
draw' params st mx my = case (mx, my) of
  (Just x, Just y) -> draw params st x y
  _ -> (0, 0, 0)

makeTopEntity 'topEntity
