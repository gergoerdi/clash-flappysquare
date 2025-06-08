{-# LANGUAGE TypeApplications, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores, LambdaCase, TupleSections #-}
module Hardware.ULX3S.Top (topEntity) where

import Hardware.ULX3S.TMDS
import Hardware.ULX3S.HDMI
import Hardware.ULX3S.ClockToBit

import Clash.Prelude
import qualified Clash.Explicit.Prelude as Ex
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA640x480

import FlappySquare.Circuit (Dom25, vDom25)
import qualified FlappySquare.Circuit as Flappy

createDomain vDom25{vName="DomHDMI", vPeriod = vPeriod vDom25 `div` 10 }
type DomVGA = Dom25

topEntity
    :: "CLK_VGA"    ::: Clock DomVGA
    -> "RESET_VGA"  ::: Reset DomVGA
    -> "CLK_HDMI"   ::: Clock DomHDMI
    -> "RESET_HDMI" ::: Reset DomHDMI
    -> "BTNS"       ::: Vec 7 (Signal DomVGA (Active High))
    -> ( "GPDI_DP"  ::: Signal DomHDMI (BitVector 4)
       , "GPDI_DN"  ::: Signal DomHDMI (BitVector 4)
       )
topEntity vgaClk vgaRst hdmiClk hdmiRst btns = differential $ fmap pack . bundle $
    (tmds_r :> tmds_g :> tmds_b :> clockToBit vgaClk :> Nil)
  where
    btn = btns!!3
    vga = Flappy.topEntity vgaClk vgaRst btn
    (tmds_r, tmds_g, tmds_b) = serializeVGA hdmiClk hdmiRst enableGen vgaClk vga

makeTopEntity 'topEntity
