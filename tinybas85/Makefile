all: tinybas85.hex

# We want to use the full undocumented 8085 instruction set
AS = asl -cpu 8085undoc -gnuerrors

tinybas85.p: tinybas85.asm
	$(AS) tinybas85.asm

tinybas85.hex: tinybas85.p
	p2hex tinybas85.p

program:
	minipro -p AM28C64B@DIP28 -f ihex -w tinybas85.hex
