# STD-85
This repository contains some programs for my STD-85 processor card designed by Craig from Bits of the Golden Age (http://bitsofthegoldenage.org/documentation/std-8085-sbc-v1-x-documentation/)


## External Requirements
### a85

### asl

### minipro

## Board Configuration

The STD-85 board is configured with a 6264 SRAM in the M1 socket and an 8264 EEPROM in the M3 socket

The memory map is
| Start Address | End Address | Description      |
|---------------|-------------|------------------|
| 0000          | 1FFF        | M1 8K SRAM       |
| C000          | DFFF        | M3 8K EEPROM     |
| E000          | FFFF        |256 byte 8155 RAM |

The IO map is configured as

| Start Port | End Port | Description      |
|------------|----------|------------------|
| 50         | 57       | 7304 dual UART   |
| E0         | E8       | 8155 IO          |
| F8         | FB       | Onboard 8153     |
| FC         | FF       | Onboard port     |

## Onboard Port Configuration
| Bit | Description |
|-----|-------------|
| Q0  | Default/Alternate Memory Map |
| Q1  | MEMEX       |
| Q2  |	IOEX        |
| Q3  |	SEG_A, J10-1|
| Q4  |	SEG_B, J10-2|
| Q5  |	SEG_C, J10-3|
| Q6  |	SEG_D, J10-4|
| Q7  |	BUZZER      |

## Jumpers and Switches
| Jumper | Shorted Pins | Description             |
|--------|--------------|-------------------------|
| J4     | 1, 2         | CLK to 8155 TIMERIN     |
| J5     | 1, 2         | Timer out (TO) to INT7.5|
| SW2    | 7,9          | M1 Default              |
| SW3    | 7,9          | M1 Alternate            |
| SW6    | x            | M2 Default              |
| SW7    | x            | M2 Alternate            |
| SW8    | 1,9          | M3 Default              |
| SW9    | 1,9          | M3 Alternate            |
| SW10   | 8,9          | 1k Default              |
| SW11   | 8,9          | 1k Alternate            |
| SW12   | 8,9          | OBP mapped to 4 io ports 0xfc-0xff, 1 needed |
| SW13   | 1,2,9        | 8155 mapped to 8 io ports 0xe0-0xe7, 6 needed |
| wire   | SW12-7       | 8153 mapped to 4 io ports 0xf8-0xc, 2 needed  |

## Onboard IO Decoding
This assumed the above 1k address range mapping

| SW12 & SW13 Pin| IO Port Range |
|----------------|---------------|
| 1              | 0xe0 - 0xe3   |
| 2              | 0xe4 - 0xe7   |
| 3              | 0xe8 - 0xeb   |
| 4              | 0xec - 0xef   |
| 5              | 0xf0 - 0xf3   |
| 6              | 0xf4 - 0xf7   |
| 7              | 0xf8 - 0xfb   |
| 8              | 0xfc - 0xff   |
| 9              | Connects to /CE |


## tinybasic2
This is Version 2 of the Tiny Basic interpreter from Li-Chan Wang.

## tinybas85
This is an enhancement of Version 2 Tiny Basic by Anders Hjelm. This version of Tiny Basic was developed to support his micro8085 system as documented in
https://hackaday.io/project/176653-micro8085
This basic adds support for PEEK, POKE, INP and OUTP as well as support for talking to SPI EEPROM memory for loading and saving the program.

The version here has been modified to work with the STD-85 board.

## tinybasic3
This is Version 3 of the Tiny Basic interpreter. This code was reconstructed from the FIXME document as I could not find any version of the assembler code in my searching. The version 3 code incorporates a number of enhancements over the version 2 code.

The eventual aim is to enhance this version with some of the changes from tinybas85

