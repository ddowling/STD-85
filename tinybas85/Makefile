all: tinybas85.hex

SERIAL=SERIAL_ONBOARD
#SERIAL=SERIAL_SOFT
#SERIAL=SERIAL_7304

# Options:
# OPTION_HELP
# OPTION_MANUAL
# OPTION_SPI
# OPTION_LOADSAVE
#OPTIONS=
OPTIONS= -D OPTION_HELP -D OPTION_MANUAL -D OPTION_SPI -D OPTION_LOADSAVE

# We want to use the full undocumented 8085 instruction set
AS = asl -cpu 8085undoc -gnuerrors -L -D ${SERIAL} ${OPTIONS}

%.p: %.asm
	$(AS) $<

%.hex: %.p
	p2hex $<

program:
	minipro -p AM28C64B@DIP28 -f ihex -w tinybas85.hex
