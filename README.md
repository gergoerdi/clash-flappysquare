# Flappy Square

This repository implements a Flappy Birds-like game in Haskell, both
as a software program (using SDL for input and graphics) and as a
hardware FPGA design via Clash.

This code is part of my talk *Clash: Haskell for FPGA Design: It's
easy as 1-2-3...419,200* presented at Haskell eXchange 2022 and Lambda
Days 2025.

See <https://unsafePerform.IO/retroclash/flappy/> for futher details,
including the slides from my talk.

## Building

Use `stack build` to build and `stack run pong` to run the software
version.

For the hardware version, use the included Shakefile. An easy way to
run it is via the provided `mk` shell script. Create a `build.mk` file
with content like the following:

    TARGET = nexys-a7-50t
    VIVADO_ROOT = /opt/somewhere/where/vivado/is/installed/bin
    ISE_ROOT = /opt/somewhere/where/ise/is/installed/ISE/bin/lin64
    QUARTUS_ROOT = /opt/somewhere/where/quartus/is/installed/bin
    
Alternatively, if you have Vivado/ISE installed in Docker or similar, you
can create a wrapper script and use that by setting `VIVADO` instead
of `VIVADO_ROOT`:

    TARGET = nexys-a7-50t
    VIVADO = /usr/local/lib/docker-scripts/xilinx-vivado-2019.1-ubuntu-18.04/run
    ISE = /usr/local/lib/docker-scripts/xilinx-ise-14.7-ubuntu-12.04/run
    QUARTUS = /usr/local/lib/docker-scripts/intel-quartus-20.1-ubuntu-20.04/run

## Supported target boards

* Xilinx ISE toolchain
  * `papilio-one`: Papilio One (Spartan-3) with the Arcade MegaBoard
  * `papilio-pro`: Papilio Pro (Spartan-6) with the Arcade MegaBoard

* Xilinx Vivado toolchain
  * `nexys-a7-50t`: Digilent Nexys A7-50T (Artix-7)
  * `basys-3`: Digilent Basys 3 (Artix-7)

* Intel Quartus toolchain
  * `de0-nano`: DE0-Nano (Cyclone IV) with Fen Logic VGA666 adapter
  * `arrow-deca`: Arrow DECA (MAX 10)

* F4PGA (formerly SymbiFlow) open source toolchain
  * `nexys-a7-50t.f4pga`: Digilent Nexys A7-50T (Artix-7)
  * `basys-3`: Digilent Basys 3 (Artix-7)

* Yosys open source toolchain
  * `ulx3s-45f` and `ulx3s-85f`: Radiona ULX3S (ECP5)

Bitfile can be built with e.g. `./mk ulx3s-85f:bitfile`. For some
targets, a rule to upload the resulting bitfile is also included,
e.g. `./mk ulx3s-85f:upload`.

Adding support for other FPGA dev boards that use the above toolchains
is very straightforward with the included Shake rules, as long as they
have VGA output and at least one input pushbutton.

Targeting other FPGA toolchains will require adding support in the
Shake rules. Alternatively, you can always just run Clash, and import
the resulting Verilog files into your FPGA toolchain in a
non-automated way.

## HDMI output

Some dev boards have HDMI output instead of VGA. There are two
different approaches implemented currently:

* The Arrow DECA has an on-board HDMI encoder, which is configured by
  the FPGA using I2C. See `target/arrow-deca/src` for the Clash
  code that does the initialization.
  
* The ULX3S has the HDMI output pins connected directly to the
  FPGA. We implement a VGA-to-HDMI serializer in `target/ulx3s/src`
  that runs at 10x the VGA pixel clock speed.

When adding support for a new board, use one of the above two sources
for guidance.
