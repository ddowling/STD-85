all: tinybas85.hex

AS = asl -cpu 8085

tinybas85.p: tinybas85.asm
	$(AS) tinybas85.asm

tinybas85.hex: tinybas85.p
	p2hex tinybas85.p

program:
	minipro -p AM28C64B@DIP28 -f ihex -w tinybas85.hex
