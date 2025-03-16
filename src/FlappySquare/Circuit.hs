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

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "BTN"       ::: Signal Dom25 (Active High)
    -> "VGA"       ::: VGAOut Dom25
topEntity clk rst = withEnableGen board clk rst
  where
    board (fmap fromActive -> btn) = vga
      where
        state = regEn initState newFrame (updateState <$> btn <*> state)
        (vga, newFrame) = video state

{-# NOINLINE video #-}
video
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 25_175_000)
    => "STATE" ::: Signal dom St
    -> ("VGA" ::: VGAOut dom
       , "NEW_FRAME" ::: Signal dom Bool
       )
video state = (vgaOut vgaSync rgb, newFrame)
  where
    VGADriver{..} = vgaDriver640x480at60
    newFrame = isFalling False (isJust <$> vgaY)
    rgb = draw <$> state <*> (fromJust <$> vgaX) <*> (fromJust <$> vgaY)

makeTopEntity 'topEntity
