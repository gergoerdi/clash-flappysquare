{-# LANGUAGE NumericUnderscores, RecordWildCards, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module FlappySquare.Circuit where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.Clock
import Data.Maybe

import RetroClash.VGA640x480
import FlappySquare

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

{-# NOINLINE topEntity #-}
topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "BTN"       ::: Signal Dom25 (Active High)
    -> "VGA"       ::: VGAOut Dom25
topEntity = withEnableGen board
  where
    board (fmap fromActive -> btn) = vgaOut vgaSync rgb
      where
        VGADriver{..} = vgaDriver
        frameEnd = isFalling False (isJust <$> vgaY)

        st = regEn initState frameEnd $ updateState <$> btn <*> st
        rgb = draw <$> st <*> (fromMaybe 0 <$> vgaX) <*> (fromMaybe 0 <$> vgaY)

makeTopEntity 'topEntity
