{-# LANGUAGE TypeApplications, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores, LambdaCase, TupleSections #-}
module Hardware.ULX3S.Top (topEntity) where

import Hardware.ULX3S.TMDS
import Hardware.ULX3S.HDMI

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
    :: "CLK_VGA"  ::: Clock DomVGA
    -> "RST_VGA"  ::: Reset DomVGA
    -> "CLK_TMDS" ::: Clock DomHDMI
    -> "RST_TMDS" ::: Reset DomHDMI
    -> "BTNS"     ::: Vec 7 (Signal DomVGA (Active High))
    -> "HDMI"     ::: HDMIOut DomHDMI
topEntity clkVGA rstVGA clkTMDS rstTMDS btns = hdmi
  where
    vga = Flappy.topEntity clkVGA rstVGA btn
    btn = btns!!3
    hdmi = vgaToHDMI clkTMDS rstTMDS enableGen clkVGA rstVGA vga

makeTopEntityWithName 'topEntity "ulxTopEntity"
