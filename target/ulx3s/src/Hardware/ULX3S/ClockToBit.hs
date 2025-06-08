{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns, OverloadedStrings, PostfixOperators, BlockArguments #-}
module Hardware.ULX3S.ClockToBit where

import Prelude
import Clash.Sized.BitVector (Bit, high, low)
import Clash.Signal.Internal (Signal, Clock (..))
import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)
import Control.Monad.State (State)
import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

clockToBit :: Clock dom -> Signal dom' Bit
clockToBit Clock{} = pure high
{-# OPAQUE clockToBit #-}
{-# ANN clockToBit hasBlackBox #-}
{-# ANN clockToBit
  let
    primName = show 'clockToBit
    tfName = show 'clockToBitF
  in InlineYamlPrimitive [VHDL, Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}

clockToBitF :: TemplateFunction
clockToBitF = TemplateFunction [0] (const True) clockToBitF#

clockToBitF# :: Backend backend => BlackBoxContext -> State backend Doc
clockToBitF# bbCtx
    | [inp] <- map fst (DSL.tInputs bbCtx)
    , [DSL.ety -> resultTy] <- DSL.tResults bbCtx
    = DSL.declarationReturn bbCtx ("clockToBit_block") do
        out <- DSL.assign "bit_o" inp
        pure [out]

    | otherwise = error $ ppShow bbCtx
