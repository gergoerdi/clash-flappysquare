{-# LANGUAGE RecordWildCards, BlockArguments #-}
import Clash.Shake
import Clash.Shake.Xilinx as Xilinx
import Clash.Shake.Intel as Intel
import qualified Clash.Shake.F4PGA as F4PGA
import Clash.Shake.ECP5 as ECP5

import Development.Shake
import Development.Shake.FilePath
import Data.Traversable (for)
import Data.Foldable (for_)

outDir :: FilePath
outDir = "_build"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } do
    useConfig "build.mk"

    phony "clean" do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    (clash, kit) <- clashRules (outDir </> "clash") Verilog
        [ "src" ]
        "FlappySquare.Circuit"
        [ "-Wno-partial-type-signatures"
        , "-fclash-intwidth=32" -- To play nicely with Spartan 3 and 6
        ] $
        return ()
    phony "clashi" $ clash ["--interactive", "src/FlappySquare/Circuit.hs"]

    let boards =
            [ ("nexys-a7-50t", "nexys-a7-50t", Xilinx.vivado nexysA750T, [])
            , ("nexys-a7-50t.f4pga", "nexys-a7-50t", F4PGA.xilinx7 nexysA750T, [])
            , ("basys-3", "basys-3", Xilinx.vivado basys3, [])
            , ("basys-3.f4pga", "basys-3", F4PGA.xilinx7 basys3, [])
            , ("papilio-pro", "papilio-pro", Xilinx.ise papilioPro, [])
            , ("papilio-one", "papilio-one", Xilinx.ise papilioOne, [])
            , ("de0-nano", "de0-nano", Intel.quartus de0Nano, [])
            , ("arrow-deca", "arrow-deca", Intel.quartus arrowDeca, ["Hardware.ArrowDeca.HDMI"])
            , ("ulx3s-45f", "ulx3s", ECP5.ecp5 "45k", [])
            , ("ulx3s-85f", "ulx3s", ECP5.ecp5 "85k", [])
            ]

    for_ boards \(name, targetName, synth, extraModules) -> do
        let targetDir = "target" </> targetName
        extraKits <- fmap mconcat $ for extraModules \extraModule -> do
            (_, extraKit) <- clashRules (outDir </> name </> "clash") Verilog
              [ "src", targetDir </> "src" ]
              extraModule
              [] $
              return ()
            return extraKit

        SynthKit{..} <- synth (kit <> extraKits) (outDir </> name </> "synth") "Top" do
            staticFiles targetDir

        mapM_ (uncurry $ nestedPhony name) $
            ("bitfile", need [bitfile]):phonies
