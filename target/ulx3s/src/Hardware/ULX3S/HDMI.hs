{-# LANGUAGE TypeApplications, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores, LambdaCase, TupleSections #-}
module Hardware.ULX3S.HDMI where

import Hardware.ULX3S.TMDS
import Hardware.ULX3S.ClockToBit

import Clash.Prelude
import qualified Clash.Explicit.Prelude as Ex
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA640x480

data HDMIOut dom = HDMIOut
    { hdmiDP :: "DP" ::: Signal dom (BitVector 4)
    , hdmiDN :: "DN" ::: Signal dom (BitVector 4)
    }

vgaToHDMI
    :: (KnownDomain hdmi, KnownDomain vga, DomainPeriod hdmi ~ DomainPeriod vga `Div` 10)
    => Clock hdmi -> Reset hdmi -> Enable hdmi
    -> Clock vga -> Reset vga
    -> VGAOut vga
    -> HDMIOut hdmi
vgaToHDMI clk rst en clkVGA rstVGA vga = HDMIOut{ hdmiDP = dp, hdmiDN = dn }
  where
    (tmds_r, tmds_g, tmds_b) = serializeVGA clk rst en clkVGA rstVGA vga
    (dp, dn) = differential $ fmap pack . bundle $ streams
    streams = clockToBit clkVGA :> tmds_r :> tmds_g :> tmds_b :> Nil

serializeVGA
    :: (KnownDomain hdmi, KnownDomain vga, DomainPeriod hdmi ~ DomainPeriod vga `Div` 10)
    => Clock hdmi -> Reset hdmi -> Enable hdmi
    -> Clock vga -> Reset vga
    -> VGAOut vga
    -> (Signal hdmi Bit, Signal hdmi Bit, Signal hdmi Bit)
serializeVGA clk rst en clkPix rstPix VGAOut{ vgaSync = VGASync{..}, ..} =
    ( serialTMDS vgaR (0, 0)
    , serialTMDS vgaG (0, 0)
    , serialTMDS vgaB (vgaVSync, vgaHSync)
    )
  where
    ser = serialize clk rst en clkPix
    serialTMDS pixel ctrl = ser $ withClockResetEnable clkPix rstPix enableGen $
        toTMDS (pack <$> pixel) (pack <$> bundle ctrl) vgaDE

-- We bind `load` without `par` in scope to make sure it is shared
-- when serializing multiple stream sin parallel.
serialize
    :: forall n ser par. (KnownDomain ser, KnownDomain par, DomainPeriod ser ~ DomainPeriod par `Div` n, KnownNat n)
    => Clock ser -> Reset ser -> Enable ser
    -> Clock par
    -> Signal par (BitVector n)
    -> Signal ser Bit
serialize clk rst en clkPar = ser
  where
    load = Ex.riseEvery clk rst en (SNat @n)
    shift = (0 +>>.)

    ser par = lsb <$> r
      where
        r = Ex.register clk rst en 0 $ mux load par' $ shift <$> r
        par' = Ex.unsafeSynchronizer clkPar clk par

differential
    :: (KnownNat n)
    => Signal dom (BitVector n)
    -> (Signal dom (BitVector n), Signal dom (BitVector n))
differential x = (pos, neg)
  where
    pos = x
    neg = complement <$> x
