all: tinybasic3_orig.hex tinybasic3.hex

AS = asl -cpu 8085undoc -gnuerrors

# Assemble and generate a listing
%.p: %.asm
	$(AS) -L $<

# Create hex file with 28 bytes to match the original listing
%.hex: %.p
	p2hex -l 28 $<

#program:
#	minipro -p AM28C64B@DIP28 -f ihex -w tinybas85.hex
