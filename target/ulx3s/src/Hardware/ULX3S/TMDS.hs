{-# LANGUAGE TypeApplications, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores, LambdaCase, TupleSections #-}
module Hardware.ULX3S.TMDS where

import Clash.Explicit.Prelude hiding (scanr1)
import Clash.Sized.Internal.BitVector (popCountBV)

toTMDS
    :: (KnownDomain system)
    => Clock system
    -> Reset system
    -> Signal system (BitVector 8)
    -> Signal system (BitVector 2)
    -> Signal system Bool
    -> Signal system (BitVector 10)
toTMDS clk rst pixel ctrl de = tmdsEncode clk rst tmdsWord
  where
    tmdsWord = mux de (Data <$> pixel) (Control <$> ctrl)

data TMDSWord
    = Data (BitVector 8)
    | Control (BitVector 2)
    deriving (Eq, Show, Generic, NFDataX)

{-# NOINLINE tmdsEncode #-}
tmdsEncode
    :: (KnownDomain dom)
    => Clock dom
    -> Reset dom
    -> Signal dom TMDSWord
    -> Signal dom (BitVector 10)
tmdsEncode clk rst = delay clk enableGen 0 . mealy clk rst enableGen tmdsEncode1 0

tmdsEncode1 :: Signed 4 -> TMDSWord -> (Signed 4, BitVector 10)
tmdsEncode1 acc = \case
  Control c -> (0, ) $ case c of
    0b00 -> 0b11_0101_0100
    0b01 -> 0b00_1010_1011
    0b10 -> 0b01_0101_0100
    0b11 -> 0b10_1010_1011
  Data d -> (acc + accN, bitCoerce (tag2, tag1, stage2))
    where
      pop = popCountBV d

      (tag1, op)
          | pop > 4 || pop == 4 && testBit d 1 = (False, xnor)
          | otherwise = (True, xor)

      stage1 :: BitVector 8
      stage1 = bitCoerce (scanr1 op (bitCoerce d))

      stage2 :: BitVector 8
      (tag2, stage2, accN) = (invert, if invert then complement stage1 else stage1, acc')
        where
          pop1 = popCountBV stage1
          popDiff :: Signed 4
          popDiff = 2 * (bitCoerce pop1 - 4)
          (invert, acc')
              | acc == 0 || pop1 == 4
              = (not tag1, if tag1 then popDiff else negate popDiff)
              | acc > 0 && pop1 > 4 || acc < 0 && pop1 < 4
              = (True, (if tag1 then 0 else 2) - popDiff)
              | otherwise
              = (False, (if tag1 then (-2) else 0) + popDiff)

scanr1 :: (a -> a -> a) -> Vec (n + 1) a -> Vec (n + 1) a
scanr1 f (xs :< x) = scanr f x xs

xnor :: Bit -> Bit -> Bit
xnor a b = complement (xor a b)
