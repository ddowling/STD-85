; 8153 REGISTERS
UARTD 	EQU 0F8H
UARTC	EQU 0F9H
	
URESET  EQU  01000000b          ;8153 RESET COMMAND
USETUP	EQU 04EH
UENABL  EQU 037H

;;; Initialise the serial port
CONINIT:
	PUSH PSW
	SUB A
        OUT UARTC              ;PUT 8251 IN COMMAND MODE
        OUT UARTC              ;WRITE 0 THREE TIMES
        OUT UARTC
        MVI A,URESET           ;RESET COMMAND
        OUT UARTC              ;WRITE IT TO 8251 USART
        MVI A,USETUP           ;8 DATA, 1 STOP, X16
        OUT UARTC              ;WRITE IT TO 8251 USART
        MVI A,UENABL           ;ERRST, RXEN, TXEN
        OUT UARTC              ;WRITE IT TO 8251 USART
	POP PSW
	RET
	
;;; Output the character in A over the 8153 serial port
CONPUT:
	PUSH B
	MOV B, A
CON0:	IN   UARTC              ;COME HERE TO DO OUTPUT
        ANI  1H                 ;STATUS BIT
        JZ   CON0               ;NOT READY, WAIT
        MOV  A, B               ;READY, GET CHAR
        OUT  UARTD              ;AND SEND IT OUT
	POP B
	RET

;;; Get a character from the serial port. Return with Z flag set if no character
CONGET: IN   UARTC
        ANI  2H                 ;MASK STATUS BIT
        RZ                      ;NOT READY, RETURN "Z"
        IN   UARTD              ;READY, READ DATA
	RET

