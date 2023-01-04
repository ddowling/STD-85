# STD-85
This repository contains some programs for my STD-85 processor card designed by Craig from Bits of the FIXME ...

## External Requirements
### a85

### asl

### minipro

## Board Configuration

The STD-85 board is configured with a 6264 SRAM in the M1 socket and an 8264 EEPROM in the M3 socket

The memory map is
0000	1fff	M1 8K SRAM

C000	DFFF	M3 8K EEPROM
E000	FFFF	256 byte 8155 RAM

The IO map is configured as

50	57	7304 dual UART
e0	e7	8155 IO
fc	ff	Onboard port

## Onboard Port Configuration
Q0	Default/Alternate Memory Map
Q1	MEMEX/D6
Q2	IOEX/D7
Q3	SEG_A/D8/J10-1
Q4	SEG_B/D9/J10-2
Q5	SEG_C/D10/J10-3
Q6	SEG_D/D11/J10-4
Q7	SPEAKER

## Jumpers

J4	1	2	CLK to 8155 TIMERIN
J5	1	2	Timer out (TO) to INT7.5




## tinybasic2
This is Version 2 of the Tiny Basic interpreter from Li-Chan Wang.

## tinybas85
This is an enhancement of Version 2 Tiny Basic that adds support for PEEK, POKE, INP and OUTP as well as support for talking to SPI EEPROM memory for loading and saving the program. It was written for the FIXME board. The version here has been modified to work with the STD-85 board

## tinybasic3
This is Version 3 of the Tiny Basic interpreter. This code was reconstructed from the FIXME document as I could not find any version of the assembler code in my searching. The version 3 code incorporates a number of enhancements over the version 2 code.
FIXME
The aim is to enhance this version with some of the changes from tinybas85

