                                ;
        ;; STD-85 Registers
STD85_CTRL EQU 0FCH
        
        ;; 7304 Registers
UART_BASE  EQU 050H
UART_BAUD  EQU UART_BASE + 0
UARTB_DATA EQU UART_BASE + 1
UARTB_CTRL EQU UART_BASE + 2
UARTA_DATA EQU UART_BASE + 3
UARTA_CTRL EQU UART_BASE + 4

        ORG 0H
START:  LXI SP, STACK
        MVI A, 00H
        OUT STD85_CTRL

        NOP
        NOP
        NOP
        NOP
        
        MVI A, 077H             ;9600 baud on both ports
        OUT UART_BAUD
        MVI A, 04EH              ;Initialize 8251A UART
        OUT UARTA_CTRL   	;1 stop bit, no parity, 8-bit char, 16x baud
        OUT UARTB_CTRL
        MVI A, 037H		;enable receive and transmit
        OUT UARTA_CTRL
        OUT UARTB_CTRL

LOOP:
        IN UARTA_CTRL           ;read status port
        ANI 1H
        JZ LOOP                 ;wait for TXRDY
        MVI A, 2
        OUT STD85_CTRL
        MVI A, 'A'
        OUT UARTA_DATA
        JMP LOOP
        
        ORG 0E000H
STACK:  DS 0
        
        END
