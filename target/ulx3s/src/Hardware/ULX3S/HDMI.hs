{-# LANGUAGE TypeApplications, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores, LambdaCase, TupleSections #-}
module Hardware.ULX3S.HDMI where

import Hardware.ULX3S.TMDS

import Clash.Prelude
import qualified Clash.Explicit.Prelude as Ex
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA640x480

serializeVGA
    :: (KnownDomain hdmi, KnownDomain vga)
    => Clock hdmi -> Reset hdmi -> Enable hdmi
    -> Clock vga
    -> VGAOut vga
    -> (Signal hdmi Bit, Signal hdmi Bit, Signal hdmi Bit)
serializeVGA clk rst en clkPix VGAOut{..} = (r, g, b)
  where
    r :> g :> b :> Nil = serialize clk rst en clkPix rgbs
    rgbs =
      (pack <$> vgaR) :>
      (pack <$> vgaG) :>
      (pack <$> vgaB) :>
      Nil

serialize
    :: (KnownDomain ser, KnownDomain par, Functor f)
    => Clock ser -> Reset ser -> Enable ser
    -> Clock par
    -> f (Signal par (BitVector 8))
    -> f (Signal ser Bit)
serialize clk rst en clkPar = fmap (fmap lsb . ser)
  where
    load = Ex.riseEvery clk rst en (SNat @10)
    shift = (0 +>>.)

    ser par =
      let r = Ex.register clk rst en 0 $ mux load (Ex.unsafeSynchronizer clkPar clk par) $ shift <$> r
      in r

differential
    :: (KnownNat n)
    => Signal dom (BitVector n)
    -> (Signal dom (BitVector n), Signal dom (BitVector n))
differential x = (pos, neg)
  where
    pos = x
    neg = complement <$> x
