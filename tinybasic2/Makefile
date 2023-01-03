all: tinybasic.hex serial_test.hex

AS85 = a85

tinybasic.hex: tinybasic.asm
	$(AS85) tinybasic.asm -O tinybasic.hex -L tinybasic.lst

serial_test.hex: serial_test.asm
	$(AS85) serial_test.asm -O serial_test.hex -L serial_test.lst

program:
	minipro -p AM28C64B@DIP28 -f ihex -w tinybasic.hex
