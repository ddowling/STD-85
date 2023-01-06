; *************************************************************
;
;                  T B I
;         TINY BASIC INTERPRETER
;                VERSION 3.0
; 	         FOR 8080 SYSTEM
;               LI-CHEN WANG
;              26 APRIL 1977
; *************************************************************
; *** MEMORY USAGE ***
; 0080-01FF are for variable, input line and stack
; 2000-3FFF are for tiny basic text & array
; F000-F7FF are for tbi code
BOTSCR EQU 00080H
TOPSCR EQU 00200H
BOTRAM EQU 02000H
CFLLMT EQU 04000H
EOTROM EQU 0F000H
;
;  DEFINE VARIABLES, BUFFER AND STACK IN RAM
; 
	ORG BOTSCR
KEYWRD:	DS 1 			;WAS INIT DONE?
TXTLMT:	DS 2			;LIMIT OF TEXT AREA
VARBGN:	DS 2*26			;TB VARIABLES A-Z
CURRNT:	DS 2			;POINTS TO CURRENT LINE
STKGOS:	DS 2			;SAVES SP IN 'GOSUB'
VARNXT:	DS 0			;TEMP STORAGE
STKIN:	DS 2			;SAVE SP IN 'INPUT'
LOPVAR:	DS 2             	;'FOR' LOOP SAVE AREA 
LOPINC:	DS 2			;INCREMENT
LOPLMP:	DS 2			;LIMIT
LOPLN:	DS 2 			;LINE NUMBER
LOPPT:	DS 2			;TEXT POINTER
RANPNT:	DS 2			;RANDOM NUMBER POINTER
	DS 1			;EXTRA BYTE FOR BUFFER
BUFFER:	DS 132			;INPUT BUFFER
BUFEND:	DS 0			;BUFFER END
	DS 4			;EXTRA BYTES FOR STACK
STKLMT:	DS 0			;SOFT LIMIT FOR STACK
	ORG TOPSCR
STACK:	DS 0			;STACK STARTS HERE
	ORG BOTRAM
TXTJNF:	DS 2
TEXT:	DS 2

;
; INITIALIZE
;
	ORG BOTRAM
INIT:	LXI SP, STACL
	CALL CTLF
	LXI H, KEYWRD		;AT POWER ON KEYWRD IS
	MVI A, 0C3H		;PROBABABLY NOT C3
	CMP M
	JZ TELL			;IT IS C3, CONTINUE
	MOV M, A		;NO, SET IT TO C3
	LXI H, DFTLMT		;AND SET DEFAULT VALUE
	SHLD TXTLMT		;IN TXTLMT
	MVI A, BOTROM		;INITIALIZE RANPRT
	STA RANPRT+1
PURGE:	LXI h,TEXT+4		;PURGE TEXT AREA
	SHLD TXTUNF
	MVI M, 0FFH
	SHLD TEXT
TELL:	LXI D, MSG		;TELL USER
	CALL PRTSTG
	JMP RSTART		;JUMP USER-INIT
MSG:	DB 'TINY '
	DB 'BASIC'
	DB ' V3.0', CR
OK:	DB 'OK, CR
WHAT:	DB 'WHAT?', CR
HOW:	DB 'HOW?', CR
SORRY:	DB 'SORRY', CR

; *** DIRECT COMMAND / TEXT COLLECTOR ***
;
; TBI PRINTS OUT "OK(CR)" AND THEN IS PROMPTS ">" AND READS A LINE.
; IF THE LINE STARTS WITH A NON-ZERO NUMBER, THIS NUMBER IS THE LINE
; NUMBER.  THE LINE NUMBER (IN 16 BIT BINARY) AND THE REST OF THE LINE
; (INCLUDING CR) IS STORED IN THE MEMORY.  IF A LINE WITH THE SAME
; LINE NUMBER IS ALREADY THERE, IT IS REPLACED BY THE NEW ONE. IF
; THE REST OF THE LINE CONSISTS OF A CR ONLY, IT IS NOT STORED AND
; ANY EXISTING LINE WITH THE SAME LINE NUMBER IS DELETED.
;
; AFTER A LINE IS INSERTED, REPLACED, OR DELETED, THE PROGRAM
; LOOPS BACK AND ASKS FOR ANOTHER LINE.  THIS LOOP WILL BE
; TERMINATED WHEN IT READS A LINE WITH ZERO OR NO LINE
; NUMBER; AND CONTROL IS TRANSFERED TO "DIRECT".
;
; TINY BASIC PROGRAM SAVE AREA STARTS AT THE MEMORY LOCATION
; LABELED "TEXT". THE END OF TEXT IS MARKED BY 2 BYTES XX FF.
; FOLLOWING THESE ARE 2 BYTES RESERVED FOR THE ARRAY ELEMENT @(0).
; THE CONTENT OF LOCATION LABELED "TXTUNF" POINTS TO ONE AFTER @(0).
;
; THE MEMORY LOCATION "CURRNT" POINTS TO THE LINE NUMBER THAT IS
; CURRENTLY BEING INTERPRETED, WHILE WE ARE IN THIS LOOP OR WHILE WE
; ARE INTERPRETING A DIRECT COMMAND (SEE NEXT SECTION), "CURRNT"
; SHOULD POINT TO A 0.
RSTART: LXI  SP,STACK
	LXI H,ST1+1		;LITERAL 0
	SHLD CURRENT		;CURRNT->LINE # = 0
ST1:    LXI  H,0
        SHLD LOPVAR
        SHLD STKGOS
	LXI D, OK
	CALL PRTSTG
ST2:	MVI  A,'>'              ;PROMPT '>' AND
        CALL GETLN              ;READ A LINE
        PUSH D                  ;DE->END OF LINE
        LXI  D,BUFFER           ;DE->BEGINNING OF LINE
        CALL TSTNUM             ;TEST IF IT IS A NUMBER
	CALL IGNBLK
        MOV  A,H                ;HL=VALUE OF THE # OR
        ORA  L                  ;0 IF NO # WAS FOUND
        POP  B                  ;BC->END OF LINE
        JZ   DIRECT
        DCX  D                  ;BACKUP DE AND SAVE
        MOV  A,H                ;VALUE OF LINE # THERE
        STAX D
        DCX  D
        MOV  A,L
        STAX D
        PUSH B                  ;BC,DE->BEGIN, END
        PUSH D
        MOV  A,C
        SUB  E
        PUSH PSW                ;A=# OF BYTES IN LINE
        CALL FNDLN              ;FIND THIS LINE IN SAVE
        PUSH D                  ;AREA, DE->SAVE AREA
        JNZ  ST3                ;NZ:NOT FOUND, INSERT
        PUSH D                  ;Z:FOUND, DELETE IT
        CALL FNDNXT             ;FIND NEXT LINE
                                ;DE->NEXT LINE
        POP  B                  ;BC->LINE TO BE DELETED
        LHLD TXTUNF             ;HL->UNFILLED SAVE AREA
        CALL MVUP               ;MOVE UP TO DELETE
        MOV  H,B                ;TXTUNF->UNFILLED AREA
        MOV  L,C
        SHLD TXTUNF             ;UPDATE
ST3:    POP  B                  ;GET READY TO INSERT
        LHLD TXTUNF             ;BUT FIRST CHECK IF
        POP  PSW                ;THE LENGTH OF NEW LINE
        PUSH H                  ;IS 3 (LINE # AND CR)
        CPI  3                  ;THEN DO NOT INSERT
        JZ   RSTART             ;MUST CLEAR THE STACK
        ADD  L                  ;COMPUTE NEW TXTUNF
	MOV E,A
        MVI  A,0
        ADC  H
        MOV  D,A                ;HL->NEW UNFILLED AREA
        LXI  D,TXTLMT           ;CHECK TO SEE IF THERE
	XCHG
	CALL COMP		; IS ENOUGH SPACE
        JNC  QSORRY             ;SORRY, NO ROOM FOR IT
        SHLD TXTUNF             ;OK, UPDATE TXTUNF
        POP  D                  ;DE->OLD UNFILLED AREA
        CALL MVDOWN
        POP  D                  ;DE->BEGIN, HL->END
        POP  H
        CALL MVUP               ;MOVE NEW LINE TO SAVE
        JMP  ST2                ;AREA
; 
; *************************************************************
; 
; *** DIRECT *** & EXEC ***
; 
; THIS SECTION OF THE CODE TESTS A STRING AGAINST A TABLE.
; WHEN A MATCH IS FOUND, CONTROL IS TRANSFERED TO THE SECTION
; OF CODE ACCORDING TO THE TABLE.
; 
; AT 'EXEC', DE SHOULD POINT TO THE STRING AND HL SHOULD POINT
; TO THE TABLE-1.  AT 'DIRECT', DE SHOULD POINT TO THE STRING.
; HL WILL BE SET UP TO POINT TO TAB1-1, WHICH IS THE TABLE OF
; ALL DIRECT AND STATEMENT COMMANDS.
; 
; A '.' IN THE STRING WILL TERMINATE THE TEST AND THE PARTIAL
; MATCH WILL BE CONSIDERED AS A MATCH.  E.G., 'P.', 'PR.',
; 'PRI.', 'PRIN.', OR 'PRINT' WILL ALL MATCH 'PRINT'.
; 
; THE TABLE CONSISTS OF ANY NUMBER OF ITEMS.  EACH ITEM
; IS A STRING OF CHARACTERS WITH BIT 7 SET TO 0 AND
; A JUMP ADDRESS STORED HI-LOW WITH BIT 7 OF THE HIGH
; BYTE SET TO 1.
; 
; END OF TABLE IS AN ITEM WITH A JUMP ADDRESS ONLY.  IF THE
; STRING DOES NOT MATCH ANY OF THE OTHER ITEMS, IT WILL
; MATCH THIS NULL ITEM AS DEFAULT.
;
DIRECT: LXI  H,TAB1-1           ;*** DIRECT ***
EXEC:   CALL IGNBLK             ;*** EXEC ***
        PUSH D                  ;SAVE POINTER
EX1:    LDAX D                  ;IF FOUND '.' IN STRING
        INX  D                  ;BEFORE ANY MISMATCH
        CPI  '.'                ;WE DECLARE A MATCH
        JZ   EX3
        INX  H                  ;HL->TABLE
        CMP  M                  ;IF MATCH, TEST NEXT
        JZ   EX1
        MVI  A,07FH             ;ELSE SEE IF BIT 7
        DCX  D                  ;OF TABLE IS SET, WHICH
        CMP  M                  ;IS THE JUMP ADDR. (HI)
        JC   EX5                ;C:YES, MATCHED
EX2:    INX  H                  ;NC:NO, FIND JUMP ADDR.
        CMP  M
        JNC  EX2
        INX  H                  ;BUMP TO NEXT TAB. ITEM
        POP  D                  ;RESTORE STRING POINTER
        JMP  EXEC               ;TEST AGAINST NEXT ITEM
EX3:    MVI  A,07FH             ;PARTIAL MATCH, FIND
EX4:    INX  H                  ;JUMP ADDR., WHICH IS
        CMP  M                  ;FLAGGED BY BIT 7
        JNC  EX4
EX5:    MOV  A,M                ;LOAD HL WITH THE JUMP
        INX  H                  ;ADDRESS FROM THE TABLE
        MOV  L,M
        ANI  7FH                ;MASK OFF BIT 7
        MOV  H,A
        POP  PSW                ;CLEAN UP THE GABAGE
        PCHL                    ;AND WE GO DO IT
;
;*************************************************************
;
; WHAT FOLLOWS IS THE CODE TO EXECUTE DIRECT AND STATEMENT
; COMMANDS.  CONTROL IS TRANSFERED TO THESE POINTS VIA THE
; COMMAND TABLE LOOKUP CODE OF 'DIRECT' AND 'EXEC' IN LAST
; SECTION.  AFTER THE COMMAND IS EXECUTED, CONTROL IS
; TRANSFERED TO OTHERS SECTIONS AS FOLLOWS:
;
; FOR 'LIST', 'NEW', AND 'STOP': GO BACK TO 'RSTART'
; FOR 'RUN': GO EXECUTE THE FIRST STORED LINE IF ANY, ELSE
; GO BACK TO 'RSTART'.
; FOR 'GOTO' AND 'GOSUB': GO EXECUTE THE TARGET LINE.
; FOR 'RETURN' AND 'NEXT': GO BACK TO SAVED RETURN LINE.
; FOR ALL OTHERS: IF 'CURRENT' -> 0, GO TO 'RSTART', ELSE
; GO EXECUTE NEXT COMMAND.  (THIS IS DONE IN 'FINISH'.)
;*************************************************************
;
; *** NEW *** STOP *** RUN (& FRIENDS) *** & GOTO ***
;
; 'NEW(CR)' RESETS 'TXTUNF'
;
; 'STOP(CR)' GOES BACK TO 'RSTART'
;
; 'RUN(CR)' FINDS THE FIRST STORED LINE, STORE ITS ADDRESS (IN
; 'CURRENT'), AND START EXECUTE IT.  NOTE THAT ONLY THOSE
; COMMANDS IN TAB2 ARE LEGAL FOR STORED PROGRAM.
;
; THERE ARE 3 MORE ENTRIES IN 'RUN':
; 'RUNNXL' FINDS NEXT LINE, STORES ITS ADDR. AND EXECUTES IT.
; 'RUNTSL' STORES THE ADDRESS OF THIS LINE AND EXECUTES IT.
; 'RUNSML' CONTINUES THE EXECUTION ON SAME LINE.
;
; 'GOTO EXPR(CR)' EVALUATES THE EXPRESSION, FIND THE TARGET
; LINE, AND JUMP TO 'RUNTSL' TO DO IT.
;
NEW:    CALL ENDCHK             ;*** NEW(CR) ***
	JMP PURGE
;
STOP:   CALL ENDCHK             ;*** STOP(CR) ***
        JMP  RSTART
;
RUN:    CALL ENDCHK             ;*** RUN(CR) ***
        LXI  D,TEXT             ;FIRST SAVED LINE
;
RUNNXL: LXI  H,0                ;*** RUNNXL ***
        CALL FNDLP              ;FIND WHATEVER LINE #
        JC   RSTART             ;C:PASSED TXTUNF, QUIT
;
RUNTSL: XCHG                    ;*** RUNTSL ***
        SHLD CURRNT             ;SET 'CURRENT'->LINE #
        XCHG
        INX  D                  ;BUMP PASS LINE #
        INX  D
;
RUNSML: CALL CHKIO              ;*** RUNSML ***
        LXI  H,TAB2-1           ;FIND COMMAND IN TAB2
        JMP  EXEC               ;AND EXECUTE IT
;
GOTO:   CALL EXPR               ;*** GOTO EXPR ***
        PUSH D                  ;SAVE FOR ERROR ROUTINE
        CALL ENDCHK             ;MUST FIND A CR
        CALL FNDLN              ;FIND THE TARGET LINE
        JNZ  AHOW               ;NO SUCH LINE #
        POP  PSW                ;CLEAR THE PUSH DE
        JMP  RUNTSL             ;GO DO IT
;
;*************************************************************
;
; *** LIST *** & PRINT ***
;
; LIST HAS THREE FORMS:
; 'LIST(CR)' LISTS ALL SAVED LINES
; 'LIST n(CR)' START LIST AT LINE N
; 'LIST N1, N2(CR) START LIST AT LINE N1 FOR N2 LINES
; YOU CAN STOP THE LISTING BY CONTROL C KEY
;
; PRINT COMMAND IS 'PRINT ....;' OR 'PRINT ....(CR)'
; WHERE '....' IS A LIST OF EXPRESIONS, FORMATS, BACK-
; ARROWS, AND STRINGS.  THESE ITEMS ARE SEPERATED BY COMMAS.
;
; A FORMAT IS A POUND SIGN FOLLOWED BY A NUMBER.  IT CONTROLS
; THE NUMBER OF SPACES THE VALUE OF A EXPRESION IS GOING TO
; BE PRINTED.  IT STAYS EFFECTIVE FOR THE REST OF THE PRINT
; COMMAND UNLESS CHANGED BY ANOTHER FORMAT.  IF NO FORMAT IS
; SPECIFIED, 6 POSITIONS WILL BE USED.
;
; A STRING IS QUOTED IN A PAIR OF SINGLE QUOTES OR A PAIR OF
; DOUBLE QUOTES.
;
; CONTROL CHARACTERS AND LOWER CASE LETTERS CAN BE INCLUDED INSIDE THE
; QUOTES. ANOTHER (BETTER) WAY OF GENERATING CONTROL CHARACTERS ON
; THE OUTPUT IS UE THE UP-ARROW CHARACRTER FOLLOWED BY A LETTER.
; ^L MEANS FF, ^I MEANS HT, ^G MEANS BELL ETC. 
;
; A (CRLF) IS GENERATED AFTER THE ENTIRE LIST HAS BEEN
; PRINTED OR IF THE LIST IS A NULL LIST.  HOWEVER IF THE LIST
; ENDED WITH A COMMA, NO (CRLF) IS GENERATED.
;
LIST:   CALL TSTNUM             ;TEST IF THERE IS A #
	PUSH H
	LXI H, 0FFFFH
	TSTC ',',LS1		;FIXME CANNOT READ??
	CALL TSTNUM
LS1:	XTHL
        CALL ENDCHK             ;IF NO # WE GET A 0
        CALL FNDLN              ;FIND THIS OR NEXT LINE
LS2:    JC   RSTART             ;C:PASSED TXTUNF
	XTHL
	MOV A,H
	ORA L
	JZ RSTART
	DCX H
	XTHL
        CALL PRTLN              ;PRINT THE LINE
	CALL PRTSTG
        CALL CHKIO              ;STOP IF HIT CONTROL-C
        CALL FNDLP              ;FIND NEXT LINE
        JMP  LS2                ;AND LOOP BACK
;
PRINT:  MVI  C,8                ;C = # OF SPACES
	TSTC '\;',PR1		;IF NULL LIST & ";"
        CALL CRLF               ;GIVE CR-LF AND
        JMP  RUNSML             ;CONTINUE SAME LINE
PR1:    TSTC @CR, PR6           ;IF NULL LIST (CR)
        CALL CRLF               ;ALSO GIVE CR-LF AND
        JMP  RUNNXL             ;GO TO NEXT LINE
PR2:    TSTC '#', PR4           ;ELSE IS IT FORMAT?
PR3:    CALL EXPR		;YES EVALUATE EXPR
	MVI A, 0C0H
	ANA L
	ORA H
	JNZ QHOW
	MOV C, L		;AND SAVE IT IN C
	JMP PR5
PR4:	CALL QTSTG		;LOOK FOR MORE TO PRINT
	JMP PR9
PR5:	TSTC ',',PR8		;IF "," GO FIND NEXT
PR6:	TSTC '*',PR7
	MVI A, ' '
	CALL OUTCH
	JMP PR6
PR7:	CALL FIN		;IN THE LIST
	JMP PR2			;LIST CONTINUES
PR8:	CALL CRLF		;LIST ENDS
	JMP FINISH
PR9:	CALL EXPR		;EVALUATE THE EXPR
	PUSH B
	CALL PRTNUM		;PRINT THE VALUE
	POP B
	JMP PR5			;MORE TO PRINT?
;
;*************************************************************
;
; *** GOSUB *** & RETURN ***
;
; 'GOSUB EXPR;' OR 'GOSUB EXPR (CR)' IS LIKE THE 'GOTO'
; COMMAND, EXCEPT THAT THE CURRENT TEXT POINTER, STACK POINTER
; ETC. ARE SAVE SO THAT EXECUTION CAN BE CONTINUED AFTER THE
; SUBROUTINE 'RETURN'.  IN ORDER THAT 'GOSUB' CAN BE NESTED
; (AND EVEN RECURSIVE), THE SAVE AREA MUST BE STACKED.
; THE STACK POINTER IS SAVED IN 'STKGOS', THE OLD 'STKGOS' IS
; SAVED IN THE STACK.  IF WE ARE IN THE MAIN ROUTINE, 'STKGOS'
; IS ZERO (THIS WAS DONE BY THE "MAIN" SECTION OF THE CODE),
; BUT WE STILL SAVE IT AS A FLAG FOR NO FURTHER 'RETURN'S.
;
; 'RETURN(CR)' UNDOS EVERYTHING THAT 'GOSUB' DID, AND THUS
; RETURN THE EXECUTION TO THE COMMAND AFTER THE MOST RECENT
; 'GOSUB'.  IF 'STKGOS' IS ZERO, IT INDICATES THAT WE
; NEVER HAD A 'GOSUB' AND IS THUS AN ERROR.
;
GOSUB:  CALL PUSHA              ;SAVE THE CURRENT "FOR"
        CAL EXPR                ;PARAMETERS
        PUSH D                  ;AND TEXT POINTER
        CALL FNDLN              ;FIND THE TARGET LINE
        JNZ  AHOW               ;NOT THERE. SAY "HOW?"
        LHLD CURRNT             ;FOUND IT, SAVE OLD
        PUSH H                  ;'CURRNT' OLD 'STKGOS'
        LHLD STKGOS
        PUSH H
        LXI  H,0                ;AND LOAD NEW ONES
        SHLD LOPVAR
        DAD  SP
        SHLD STKGOS
        JMP  RUNTSL             ;THEN RUN THAT LINE
RETURN: CALL ENDCHK             ;THERE MUST BE A CR
        LHLD STKGOS             ;OLD STACK POINTER
        MOV  A,H                ;0 MEANS NOT EXIST
        ORA  L
        JZ   QWHAT              ;SO, WE SAY: "WHAT?"
        SPHL                    ;ELSE, RESTORE IT
RESTOR:	POP  H
        SHLD STKGOS             ;AND THE OLD 'STKGOS'
        POP  H
        SHLD CURRNT             ;AND THE OLD 'CURRNT'
        POP  D                  ;OLD TEXT POINTER
        CALL POPA               ;OLD "FOR" PARAMETERS
        JMP FINISH
;
;*************************************************************
;
; *** FOR *** & NEXT ***
;
; 'FOR' HAS TWO FORMS:
; 'FOR VAR=EXP1 TO EXP2 STEP EXP3' AND 'FOR VAR=EXP1 TO EXP2'
; THE SECOND FORM MEANS THE SAME THING AS THE FIRST FORM WITH
; EXP3=1.  (I.E., WITH A STEP OF +1.)
; TBI WILL FIND THE VARIABLE VAR, AND SET ITS VALUE TO THE
; CURRENT VALUE OF EXP1.  IT ALSO EVALUATES EXP2 AND EXP3
; AND SAVE ALL THESE TOGETHER WITH THE TEXT POINTER ETC. IN
; THE 'FOR' SAVE AREA, WHICH CONSISTS OF 'LOPVAR', 'LOPINC',
; 'LOPLMT', 'LOPLN', AND 'LOPPT'.  IF THERE IS ALREADY SOME-
; THING IN THE SAVE AREA (THIS IS INDICATED BY A NON-ZERO
; 'LOPVAR'), THEN THE OLD SAVE AREA IS SAVED IN THE STACK
; BEFORE THE NEW ONE OVERWRITES IT.
; TBI WILL THEN DIG IN THE STACK AND FIND OUT IF THIS SAME
; VARIABLE WAS USED IN ANOTHER CURRENTLY ACTIVE 'FOR' LOOP.
; IF THAT IS THE CASE, THEN THE OLD 'FOR' LOOP IS DEACTIVATED.
; (PURGED FROM THE STACK..)
;
; 'NEXT VAR' SERVES AS THE LOGICAL (NOT NECESSARILLY PHYSICAL)
; END OF THE 'FOR' LOOP.  THE CONTROL VARIABLE VAR. IS CHECKED
; WITH THE 'LOPVAR'.  IF THEY ARE NOT THE SAME, TBI DIGS IN
; THE STACK TO FIND THE RIGHT ONE AND PURGES ALL THOSE THAT
; DID NOT MATCH.  EITHER WAY, TBI THEN ADDS THE 'STEP' TO
; THAT VARIABLE AND CHECK THE RESULT WITH THE LIMIT.  IF IT
; IS WITHIN THE LIMIT, CONTROL LOOPS BACK TO THE COMMAND
; FOLLOWING THE 'FOR'.  IF OUTSIDE THE LIMIT, THE SAVE AREA
; IS PURGED AND EXECUTION CONTINUES.
;
FOR:    CALL PUSHA              ;SAVE THE OLD SAVE AREA
        CALL SETVAL             ;SET THE CONTROL VAR.
        DCX  H                  ;HL IS ITS ADDRESS
        SHLD LOPVAR             ;SAVE THAT
        LXI  H,TAB4-1           ;USE 'EXEC' TO LOOK
        JMP  EXEC               ;FOR THE WORD 'TO'
FR1:    CALL EXPR               ;EVALUATE THE LIMIT
        SHLD LOPLMT             ;SAVE THAT
        LXI  H,TAB5-1           ;USE 'EXEC' TO LOOK
        JMP EXEC                ;FOR THE WORD 'STEP'
FR2:    CALL EXPR               ;FOUND IT, GET STEP
        JMP  FR4
FR3:    LXI  H,1H               ;NOT FOUND, SET TO 1
FR4:    SHLD LOPINC             ;SAVE THAT TOO
        LHLD CURRNT             ;SAVE CURRENT LINE #
        SHLD LOPLN
        XCHG                    ;AND TEXT POINTER
        SHLD LOPPT
        LXI  B,0AH              ;DIG INTO STACK TO
        LHLD LOPVAR             ;FIND 'LOPVAR'
        XCHG
        MOV  H,B
        MOV  L,B                ;HL=0 NOW
        DAD  SP                 ;HERE IS THE STACK
	JMP FR6
FR5:	DAD  B                  ;EACH LEVEL IS 10 DEEP
FR6:	MOV  A,M                ;GET THAT OLD 'LOPVAR'
        INX  H
        ORA  M
        JZ   FR7                ;0 SAYS NO MORE IN IT
        MOV  A,M
        DCX  H
        CMP  D                  ;SAME AS THIS ONE?
        JNZ  FR5
        MOV  A,M                ;THE OTHER HALF?
        CMP  E
        JNZ  FR5
        XCHG                    ;YES, FOUND ONE
        LXI  H,0H
        DAD  SP                 ;TRY TO MOVE SP
        MOV  B,H
        MOV  C,L
        LXI  H,0AH
        DAD  D
        CALL MVDOWN             ;AND PURGE 10 WORDS
        SPHL                    ;IN THE STACK
FR7:    LHLD LOPPT              ;JOB DONE, RESTORE DE
        XCHG
        JMP FINISH              ;AND CONTINUE
;
NEXT:   CALL ISTV               ;GET ADDRESS OF VAR.
        JC   QWHAT              ;NO VARIABLE, "WHAT?"
        SHLD VARNXT             ;YES, SAVE IT
NX1:    PUSH D                  ;SAVE TEXT POINTER
        XCHG
        LHLD LOPVAR             ;GET VAR. IN 'FOR'
        MOV  A,H
        ORA  L                  ;0 SAYS NEVER HAD ONE
        JZ   AWHAT              ;SO WE ASK: "WHAT?"
        CALL COMP               ;ELSE WE CHECK THEM
        JZ   NX2                ;OK, THEY AGREE
        POP  D                  ;NO, LET'S SEE
        CALL POPA               ;PURGE CURRENT LOOP
        LHLD VARNXT             ;AND POP ONE LEVEL
        JMP  NX1                ;GO CHECK AGAIN
NX2:    MOV  E,M                ;COME HERE WHEN AGREED
        INX  H
        MOV  D,M                ;DE=VALUE OF VAR.
        LHLD LOPINC
        PUSH H
        MOV  A,H
        XRA  D
        MOV  A,D
        DAD  D                  ;ADD ONE STEP
        JM   NX3
        XRA  H
        JM   NX5
NX3:    XCHG
        LHLD LOPVAR             ;PUT IT BACK
        MOV  M,E
        INX  H
        MOV  M,D
        LHLD LOPLMT             ;HL->LIMIT
        POP  PSW                ;OLD HL
        ORA  
        JP   NX4                ;STEP > 0
        XCHG                    ;STEP < 0
NX4:    CALL CKHLDE             ;COMPARE WITH LIMIT
        POP  D                  ;RESTORE TEXT POINTER
        JC   NX6                ;OUTSIDE LIMIT
        LHLD LOPLN              ;WITHIN LIMIT, GO
        SHLD CURRNT             ;BACK TO THE SAVED
        LHLD LOPPT              ;'CURRNT' AND TEXT
        XCHG                    ;POINTER
        JMP FINISH
NX5:    POP  H
        POP  D
NX2:    CALL POPA               ;PURGE THIS LOOP
        JMP FINISH
;
;*************************************************************
;
; *** REM *** IF *** INPUT *** & LET (& DEFLT) ***
;
; 'REM' CAN BE FOLLOWED BY ANYTHING AND IS IGNORED BY TBI.
; TBI TREATS IT LIKE AN 'IF' WITH A FALSE CONDITION.
;
; 'IF' IS FOLLOWED BY AN EXPR. AS A CONDITION AND ONE OR MORE
; COMMANDS (INCLUDING OTHER 'IF'S) SEPERATED BY SEMI-COLONS.
; NOTE THAT THE WORD 'THEN' IS NOT USED.  TBI EVALUATES THE
; EXPR. IF IT IS NON-ZERO, EXECUTION CONTINUES.  IF THE
; EXPR. IS ZERO, THE COMMANDS THAT FOLLOWS ARE IGNORED AND
; EXECUTION CONTINUES AT THE NEXT LINE.
;
; 'INPUT' COMMAND IS LIKE THE 'PRINT' COMMAND, AND IS FOLLOWED
; BY A LIST OF ITEMS.  IF THE ITEM IS A STRING IN SINGLE OR
; DOUBLE QUOTES, OR IS A BACK-ARROW, IT HAS THE SAME EFFECT AS
; IN 'PRINT'.  IF AN ITEM IS A VARIABLE, THIS VARIABLE NAME IS
; PRINTED OUT FOLLOWED BY A COLON.  THEN TBI WAITS FOR AN
; EXPR. TO BE TYPED IN.  THE VARIABLE IS THEN SET TO THE
; VALUE OF THIS EXPR.  IF THE VARIABLE IS PROCEDED BY A STRING
; (AGAIN IN SINGLE OR DOUBLE QUOTES), THE STRING WILL BE
; PRINTED FOLLOWED BY A COLON.  TBI THEN WAITS FOR INPUT EXPR.
; AND SET THE VARIABLE TO THE VALUE OF THE EXPR.
;
; IF THE INPUT EXPR. IS INVALID, TBI WILL PRINT "WHAT?",
; "HOW?" OR "SORRY" AND REPRINT THE PROMPT AND REDO THE INPUT.
; THE EXECUTION WILL NOT TERMINATE UNLESS YOU TYPE CONTROL-C.
; THIS IS HANDLED IN 'INPERR'.
;
; 'LET' IS FOLLOWED BY A LIST OF ITEMS SEPERATED BY COMMAS.
; EACH ITEM CONSISTS OF A VARIABLE, AN EQUAL SIGN, AND AN EXPR.
; TBI EVALUATES THE EXPR. AND SET THE VARIABLE TO THAT VALUE.
; TBI WILL ALSO HANDLE 'LET' COMMAND WITHOUT THE WORD 'LET'.
; THIS IS DONE BY 'DEFLT'.
;
REM:    LXI  H,0H               ;*** REM ***
        JMP IF1                 ;THIS IS LIKE 'IF 0'

IFF:    CALL EXPR               ;*** IF ***
        MOV  A,H                ;IS THE EXPR.=0?
        ORA  L
        JNZ  RUNSML             ;NO, CONTINUE
        CALL FNDSKP             ;YES, SKIP REST OF LINE
        JNC  RUNTSL             ;AND RUN THE NEXT LINE
        JMP  RSTART             ;IF NO NEXT, RE-START
;
INPERR: LHLD STKINP             ;*** INPERR ***
        SPHL                    ;RESTORE OLD SP
        POP  H                  ;AND OLD 'CURRNT'
        SHLD CURRNT
        POP  D                  ;AND OLD TEXT POINTER
        POP  D                  ;REDO INPUT
;
INPUT:                          ;*** INPUT ***
IP1:    PUSH D                  ;SAVE IN CASE OF ERROR
        CALL QTSTG              ;IS NEXT ITEM A STRING?
        JMP  IP8                ;NO
IP2:	CALL IPTV               ;YES, BUT FOLLOWED BY A
        JC   IP5                ;VARIABLE?   NO.
IP3:	CALL IP12
	LXI D,BUFFER
	CALL EXPR
	CALL ENDCHK
	POP D			;OK GET OLD HL
	XCHG
	MOV H,E
	INX H
	MOV M,D
IP4:	POP H			;GET OLD "CURRENT"
	SHLD CURRENT
	POP D			;AND OLD TEXT POINTER
IP5:	POP PSW			;PURGE JUNK IN STACK
IP6:	TSTC ",",IP7		;IS NEXT CH ","?
	JMP INPUT
IP7:	JMP FINISH
IP8:	PUSH D			;SAVE FOR "PRTSTG"
	CALL TSTV		;MUST BE VARIABLE NOW
	JNC JP11
IP10:	JMP QWHAT		;"WHAT?" IT IS NOT?
IP11:	MOV B,E
	POP D
	CALL PRTCHS		;PRINT THOSE AS PROMPT
	JMP IP3
IP12:	POP B			;RETURN ADDRESS
	PUSH D			;SAVE TEXT POINTER
	XCHG
	LHLD CURRNT		;ALSO SAVE "CURRENT"
	PUSH H
	LXI H,IP1		;A NEGATIVE NUMBER
	SHLD CURRNT		;AS A FLAG
	LXI H,0			;SAVE SP TOO
	DAD SP
	SHLD STKINP
	PUSH D			;OLD HL
	MVI A,' '		;PRINT A SPACE
	PUSH B
	JMP GETLN
;
DEFLT:  LDAX D                  ;***  DEFLT ***
        CPI  CR                 ;EMPTY LINE IS OK
        JZ   LT4                ;ELSE IT IS 'LET'
;
LET:
LT2:	CALL SETVAL             ;*** LET ***
LT3:	TSTC ",",LT4            ;SET VALUE TO VAR.
        JMP  LET                ;ITEM BY ITEM
LT4:    JMP FINISH              ;UNTIL FINISH
;
;*************************************************************
;
; *** EXPR ***
;
; 'EXPR' EVALUATES ARITHMETICAL OR LOGICAL EXPRESSIONS.
; <EXPR>::<EXPR1>
;         <EXPR1><REL.OP.><EXPR1>
; WHERE <REL.OP.> IS ONE OF THE OPERATORS IN TAB6 AND THE
; RESULT OF THESE OPERATIONS IS 1 IF TRUE AND 0 IF FALSE.
; <EXPR1>::=(+ OR -)<EXPR2>(+ OR -<EXPR2>)(....)
; WHERE () ARE OPTIONAL AND (....) ARE OPTIONAL REPEATS.
; <EXPR2>::=<EXPR3>(* OR /><EXPR3>)(....)
; <EXPR3>::=<VARIABLE>
;           <FUNCTION>
;           (<EXPR>)
; <EXPR> IS RECURSIVE SO THAT VARIABLE '@' CAN HAVE AN <EXPR>
; AS INDEX, FUNCTIONS CAN HAVE AN <EXPR> AS ARGUMENTS, AND
; <EXPR3> CAN BE AN <EXPR> IN PARANTHESE.
;
EXPR:   CALL EXPR1              ;*** EXPR ***
        PUSH H                  ;SAVE <EXPR1> VALUE
        LXI  H,TAB6-1           ;LOOKUP REL.OP.
        JMP  EXEC               ;GO DO IT
XPR1:   CALL XPR8               ;REL.OP.">="
        RC                      ;NO, RETURN HL=0
        MOV  L,A                ;YES, RETURN HL=1
        RET
XPR2:   CALL XPR8               ;REL.OP."#"
        RZ                      ;FALSE, RETURN HL=0
        MOV  L,A                ;TRUE, RETURN HL=1
        RET
XPR3:   CALL XPR8               ;REL.OP.">"
        RZ                      ;FALSE
        RC                      ;ALSO FALSE, HL=0
        MOV  L,A                ;TRUE, HL=1
        RET
XPR4:   CALL XPR8               ;REL.OP."<="
        MOV  L,A                ;SET HL=1
        RZ                      ;REL. TRUE, RETURN
        RC
        MOV  L,H                ;ELSE SET HL=0
        RET
XPR5:   CALL XPR8               ;REL.OP."="
        RNZ                     ;FALSE, RETURN HL=0
        MOV  L,A                ;ELSE SET HL=1
        RET
XPR6:   CALL XPR8               ;REL.OP."<"
        RNC                     ;FALSE, RETURN HL=0
        MOV  L,A                ;ELSE SET HL=1
        RET
XPR7:   POP  H                  ;NOT .REL.OP
        RET                     ;RETURN HL=<EXPR2>
XPR8:   MOV  A,C                ;SUBROUTINE FOR ALL
        POP  H                  ;REL.OP.'S
        POP  B
        PUSH H                  ;REVERSE TOP OF STACK
        PUSH B
        MOV  C,A
        CALL EXPR1              ;GET 2ND <EXPR2>
        XCHG                    ;VALUE IN DE NOW
        XTHL                    ;1ST <EXPR2> IN HL
        CALL CKHLDE             ;COMPARE 1ST WITH 2ND
        POP  D                  ;RESTORE TEXT POINTER
        LXI  H,0H               ;SET HL=0, A=1
        MVI  A,1
        RET
;
EXPR1:  TSTC '-',XPR1           ;NEGATIVE SIGN?
        LXI  H,0H               ;YES, FAKE '0-'
        JMP  XP16               ;TREAT LIKE SUBTRACT
XP11:   TST '+',XP12            ;POSITIVE SIGN? IGNORE
XP12:   CALL EXPR2              ;1ST <EXPR2>
XP13:   TSTC '+',XP15           ;ADD?
        PUSH H                  ;YES, SAVE VALUE
        CALL EXPR2              ;GET 2ND <EXPR3>
XP14:   XCHG                    ;2ND IN DE
        XTHL                    ;1ST IN HL
        MOV  A,H                ;COMPARE SIGN
        XRA  D
        MOV  A,D
        DAD  D
        POP  D                  ;RESTORE TEXT POINTER
        JM   XP13               ;1ST AND 2ND SIGN DIFFER
        XRA  H                  ;1ST AND 2ND SIGN EQUAL
        JP   XP13               ;SO IS RESULT
        JMP  QHOW               ;ELSE WE HAVE OVERFLOW
XP25:   TSTC '-',XPR9           ;SUBTRACT?
XP26:   PUSH H                  ;YES, SAVE 1ST <EXPR2>
        CALL EXPR2              ;GET 2ND <EXPR2>
        CALL CHGSGN             ;NEGATE
        JMP  XP14               ;AND ADD THEM

EXPR2:  CALL EXPR3              ;GET 1ST <EXPR3>
XP31:   TSTC '*',XP24           ;MULTIPLY?
        PUSH H                  ;YES, SAVE 1ST
        CALL EXPR3              ;AND GET 2ND <EXPR4>
        MVI  B,0H               ;CLEAR B FOR SIGN
        CALL CHKSGN             ;CHECK SIGN
        XTHL                    ;1ST IN HL
        CALL CHKSGN             ;CHECK SIGN OF 1ST
        XCHG
        XTHL
        MOV  A,H                ;IS HL > 255 ?
        ORA  A
        JZ   XP22               ;NO
        MOV  A,D                ;YES, HOW ABOUT DE
        ORA  D
        XCHG                    ;PUT SMALLER IN HL
        JNZ  AHOW               ;ALSO >, WILL OVERFLOW
XP22:   MOV  A,L                ;THIS IS DUMB
        LXI  H,0H               ;CLEAR RESULT
        ORA  A                  ;ADD AND COUNT
        JZ   XP25
XP23:   DAD  D
        JC   AHOW               ;OVERFLOW
        DCR  A
        JNZ  XP23
        JMP  XP25               ;FINISHED
XP24:   TSTC '/',XPR9           ;DIVIDE?
        PUSH H                  ;YES, SAVE 1ST <EXPR3>
        CALL EXPR3              ;AND GET THE SECOND ONE
        MVI  B,0H               ;CLEAR B FOR SIGN
        CALL CHKSGN             ;CHECK SIGN OF 2ND
        XTHL                    ;GET 1ST IN HL
        CALL CHKSGN             ;CHECK SIGN OF 1ST
        XCHG
        XTHL
        XCHG
        MOV  A,D                ;DIVIDE BY 0?
        ORA  E
        JZ   AHOW               ;SAY "HOW?"
        PUSH B                  ;ELSE SAVE SIGN
        CALL DIVIDE             ;USE SUBROUTINE
        MOV  H,B                ;RESULT IN HL NOW
        MOV  L,C
        POP  B                  ;GET SIGN BACK
XP25:   POP  D                  ;AND TEXT POINTER
        MOV  A,H                ;HL MUST BE +
        ORA  A
        JM   QHOW               ;ELSE IT IS OVERFLOW
        MOV  A,B
        ORA  A
        CM   CHGSGN             ;CHANGE SIGN IF NEEDED
        JMP  XP21               ;LOOK FOR MORE TERMS
;
EXPR3:  LXI  H,TAB4-1           ;FIND FUNCTION IN TAB4
        JMP  EXEC               ;AND GO DO IT
NOTF:   CALL TSTV               ;NO, NOT A FUNCTION
        JC   XP32               ;NOR A VARIABLE
        MOV  A,M                ;VARIABLE
        INX  H
        MOV  H,M                ;VALUE IN HL
        MOV  L,A
        RET
XP32:   CALL TSTNUM             ;OR IS IT A NUMBER
        MOV  A,B                ;# OF DIGIT
        ORA  A
        RNZ                     ;OK
PARN:   TSTC '(',XPR0		;NO DIGIT MUST BE
        CALL EXPR               ;"(EXPR)"
	TSTC ')',XPR0				
XPR9:   RET
XPR0:   JMP  QWHAT              ;ELSE SAY: "WHAT?"
;
RND:    CALL PARN               ;*** RND(EXPR) ***
        MOV  A,H                ;EXPR MUST BE +
        ORA  A
        JM   QHOW
        ORA  L                  ;AND NON-ZERO
        JZ   QHOW
        PUSH D                  ;SAVE BOTH
        PUSH H
        LHLD RANPNT             ;GET MEMORY AS RANDOM
        LXI  D,RANEND           ;NUMBER
        CALL COMP
        JC   RA1                ;WRAP AROUND IF LAST
        LXI  H,BOTROM
RA1:    MOV  E,M
        INX  H
        MOV  D,M
        SHLD RANPNT
        POP  H
        XCHG
        PUSH B
        CALL DIVIDE             ;RND(N)=MOD(M,N)+1
        POP  B
        POP  D
        INX  H
        RET
;
ABS:    CALL PARN               ;*** ABS(EXPR) ***
        DCX  D
        CALL CHKSGN             ;CHECK SIGN
        INX  D
        RET
;
SIZE:   LHLD TXTUNF             ;*** SIZE ***
        PUSH D                  ;GET THE NUMBER OF FREE
        XCHG                    ;BYTES BETWEEN 'TXTUNF'
        LXI  H,VARBGN           ;AND 'VARBGN'
        CALL SUBDE
        POP  D
        RET
;
;*************************************************************
;
; *** DIVIDE *** SUBDE *** CHKSGN *** CHGSGN *** & CKHLDE ***
;
; 'DIVIDE' DIVIDES HL BY DE, RESULT IN BC, REMAINDER IN HL
;
; 'SUBDE' SUBSTRACTS DE FROM HL
;
; 'CHKSGN' CHECKS SIGN OF HL.  IF +, NO CHANGE.  IF -, CHANGE
; SIGN AND FLIP SIGN OF B.
;
; 'CHGSGN' CHECKS SIGN N OF HL AND B UNCONDITIONALLY.
;
; 'CKHLDE' CHECKS SIGN OF HL AND DE.  IF DIFFERENT, HL AND DE
; ARE INTERCHANGED.  IF SAME SIGN, NOT INTERCHANGED.  EITHER
; CASE, HL DE ARE THEN COMPARED TO SET THE FLAGS.
;
DIVIDE: PUSH H                  ;*** DIVIDE ***
        MOV  L,H                ;DIVIDE H BY DE
        MVI  H,0
        CALL DV1
        MOV  B,C                ;SAVE RESULT IN B
        MOV  A,L                ;(REMINDER+L)/DE
        POP  H
        MOV  H,A
DV1:    MVI  C,0FFH             ;RESULT IN C
DV2:    INR  C                  ;DUMB ROUTINE
        CALL SUBDE              ;DIVIDE BY SUBTRACT
        JNC  DV2                ;AND COUNT
        DAD  D
        RET
;
SUBDE:  MOV  A,L                ;*** SUBDE ***
        SUB  E                  ;SUBSTRACT DE FROM
        MOV  L,A                ;HL
        MOV  A,H
        SBB  D
        MOV  H,A
        RET
;
CHKSGN: MOV  A,H                ;*** CHKSGN ***
        ORA  A                  ;CHECK SIGN OF HL
        RP                      ;IF -, CHANGE SIGN
;
CHGSGN: MOV  A,H                ;*** CHGSGN ***
	ORA L
	RZ
	MOV A,H
        PUSH PSW
        CMA                     ;CHANGE SIGN OF HL
        MOV  H,A
        MOV  A,L
        CMA
        MOV  L,A
        INX  H
        POP  PSW
        XRA  H
        JP   QHOW
        MOV  A,B                ;AND ALSO FLIP B
        XRI  80H
        MOV  B,A
        RET
;
CKHLDE: MOV  A,H
        XRA  D                  ;SAME SIGN?
        JP   CK1                ;YES, COMPARE
        XCHG                    ;NO, XCH AND COMP
CK1:    CALL COMP
        RET
COMP:	MOV A,H			;*** COMP***
	CMP D			;COMPARE HL WITH DE
	RNZ			;RETURN CORRECT C AND
	MOV A,L			;Z FLAGS
	CMP E			;BUT OLD A IS LOST
	RET
;
;*************************************************************
;
; *** SETVAL *** FIN *** ENDCHK *** & ERROR (& FRIENDS) ***
;
; "SETVAL" EXPECTS A VARIABLE, FOLLOWED BY AN EQUAL SIGN AND
; THEN AN EXPR.  IT EVALUATES THE EXPR. AND SET THE VARIABLE
; TO THAT VALUE.
;
; "FIN" CHECKS THE END OF A COMMAND.  IF IT ENDED WITH ";",
; EXECUTION CONTINUES.  IF IT ENDED WITH A CR, IT FINDS THE
; NEXT LINE AND CONTINUE FROM THERE.
;
; "ENDCHK" CHECKS IF A COMMAND IS ENDED WITH CR.  THIS IS
; REQUIRED IN CERTAIN COMMANDS.  (GOTO, RETURN, AND STOP ETC.)
;
; "ERROR" PRINTS THE STRING POINTED BY DE (AND ENDS WITH CR).
; IT THEN PRINTS THE LINE POINTED BY 'CURRNT' WITH A "?"
; INSERTED AT WHERE THE OLD TEXT POINTER (SHOULD BE ON TOP
; OF THE STACK) POINTS TO.  EXECUTION OF TB IS STOPPED
; AND TBI IS RESTARTED.  HOWEVER, IF 'CURRNT' -> ZERO
; (INDICATING A DIRECT COMMAND), THE DIRECT COMMAND IS NOT
; PRINTED.  AND IF 'CURRNT' -> NEGATIVE # (INDICATING 'INPUT'
; COMMAND), THE INPUT LINE IS NOT PRINTED AND EXECUTION IS
; NOT TERMINATED BUT CONTINUED AT 'INPERR'.
;
; RELATED TO 'ERROR' ARE THE FOLLOWING:
; 'QWHAT' SAVES TEXT POINTER IN STACK AND GET MESSAGE "WHAT?"
; 'AWHAT' JUST GET MESSAGE "WHAT?" AND JUMP TO 'ERROR'.
; 'QSORRY' AND 'ASORRY' DO SAME KIND OF THING.
; 'QHOW' AND 'AHOW' IN THE ZERO PAGE SECTION ALSO DO THIS.
;
SETVAL: CALL TSTV               ;*** SETVAL ***
        JC   QWHAT              ;"WHAT?" NO VARIABLE
        PUSH H                  ;SAVE ADDRESS OF VAR.
        tstc '=',SV1            ;PASS "=" SIGN
        CALL EXPR               ;EVALUATE EXPR.
        MOV  B,H                ;VALUE IS IN BC NOW
        MOV  C,L
        POP  H                  ;GET ADDRESS
        MOV  M,C                ;SAVE VALUE
        INX  H
        MOV  M,B
        RET
; 
FINISH:	CALL FIN		;CHECK END OF COMMAND
SV1:    JMP  QWHAT              ;NO "=" SIGN
;
FIN:    TSTC '\;',F11		;*** FIN *** 
        POP  PSW                ;";", PURGE RET. ADDR.
        JMP  RUNSML             ;CONTINUE SAME LINE
FI1:    TSTC CR,F12             ;NOT ";", IS IT CR?
        POP  PSW                ;YES, PURGE RET. ADDR.
        JMP  RUNNXL             ;RUN NEXT LINE
FI2:    RET                     ;ELSE RETURN TO CALLER
;
IGNBLK:	LDAX D			;***IGNBLK ***
	CPI ' '			;IGNORE BLANKS
	RNZ			;IN TEXT WHERE DE->
	INX D			;AND RETURN THE FIRST
	JMP IGNBLK		;NON0BLANK CHAR IN A
ENDCHK: CALL IGNBLK             ;*** ENDCHK ***
        CPI  CR                 ;END WITH CR?
        RZ                      ;OK, ELSE SAY: "WHAT?"
;
QWHAT:  PUSH D                  ;*** QWHAT ***
AWHAT:  LXI  D,WHAT             ;*** AWHAT ***
ERROR:  CALL CRLF               ;*** ERROR ***
        CALL PRTSTG             ;PRINT THE ERROR MESSAGE
        LHLD CURRNT             ;GET CURRENT LINE #
        PUSH H
        MOV  A,M                ;CHECK THE VALUE
        INX  H
        ORA  M
        POP  D
        JZ   TELL               ;IF ZERO, JUST RESTART
        MOV  A,M                ;IF NEGATIVE,
        ORA  A
        JM   INPERR             ;REDO INPUT
        CALL PRTLN              ;ELSE PRINT THE LINE
	POP B
	MOV B,C
	CALL PRTCHS
        MVI  A,3FH              ;PRINT A "?"
        CALL OUTCH
	CALL PTRSTG
	JMP TELL
;
QSORRY: PUSH D                  ;*** QSORRY ***
ASORRY: LXI  D,SORRY            ;*** ASORRY ***
        JMP  ERROR
;*************************************************************
;
; *** FNDLN (& FRIENDS) ***
;
; 'FNDLN' FINDS A LINE WITH A GIVEN LINE # (IN HL) IN THE
; TEXT SAVE AREA.  DE IS USED AS THE TEXT POINTER.  IF THE
; LINE IS FOUND, DE WILL POINT TO THE BEGINNING OF THAT LINE
; (I.E., THE LOW BYTE OF THE LINE #), AND FLAGS ARE NC & Z.
; IF THAT LINE IS NOT THERE AND A LINE WITH A HIGHER LINE #
; IS FOUND, DE POINTS TO THERE AND FLAGS ARE NC & NZ.  IF
; WE REACHED THE END OF TEXT SAVE AREA AND CANNOT FIND THE
; LINE, FLAGS ARE C & NZ.
; 'FNDLN' WILL INITIALIZE DE TO THE BEGINNING OF THE TEXT SAVE
; AREA TO START THE SEARCH.  SOME OTHER ENTRIES OF THIS
; ROUTINE WILL NOT INITIALIZE DE AND DO THE SEARCH.
; 'FNDLNP' WILL START WITH DE AND SEARCH FOR THE LINE #.
; 'FNDNXT' WILL BUMP DE BY 2, FIND A CR AND THEN START SEARCH.
; 'FNDSKP' USE DE TO FIND A CR, AND THEN START SEARCH.
;
FNDLN:  MOV  A,H                ;*** FNDLN ***
        ORA  A                  ;CHECK SIGN OF HL
        JM   QHOW               ;IT CANNOT BE -
        LXI  D,TEXT             ;INIT TEXT POINTER
;
FNDLP:	INX D			;IS IT EOT MARK
	LDAX D
	DCX D
	ADD A
        RC                      ;C,NZ PASSED END
        LDAX D                  ;WE DID NOT, GET BYTE 1
        SUB  L                  ;IS THIS THE LINE?
        MOV  B,A                ;COMPARE LOW ORDER
        INX  D
        LDAX D                  ;GET BYTE 2
        SBB  H                  ;COMPARE HIGH ORDER
        JC   FL1                ;NO, NOT THERE YET
        DCX  D                  ;ELSE WE EITHER FOUND
        ORA  B                  ;IT, OR IT IS NOT THERE
        RET                     ;NC,Z:FOUND, NC,NZ:NO
;
FNDNXT:                         ;*** FNDNXT ***
        INX  D                  ;FIND NEXT LINE
FL1:    INX  D                  ;JUST PASSED BYTE 1 & 2
;
FNDSKP: LDAX D                  ;*** FNDSKP ***
        CPI  CR                 ;TRY TO FIND CR
        JNZ  FL1                ;KEEP LOOKING
        INX  D                  ;FOUND CR, SKIP OVER
        JMP  FNDLP              ;CHECK IF END OF TEXT
;
TSTV:	CALL IGNBLK		;*** TSTV ***
	SUI '@'			;TEST VARIABLES
	RC			;C:NOT A VARIABLE
	JNZ TV1			;NOT "@" ARRAY
	INX D			;IT IS THE "@" ARRAY
	CALL PARN		;@ SHOULD BE FOLLOWED
        DAD  H                  ;BY (EXPR) AS ITS INDEX
        JC   QHOW               ;IS INDEX TOO BIG?
        PUSH D                  ;WILL IT OVERWRITE
        XCHG                    ;TEXT?
        CALL SIZE               ;FIND SIZE OF FREE
        CALL COMP               ;AND CHECK THAT
        JC   ASORRY             ;IF SO, SAY "SORRY"
	CALL LOCR		;IF FITS GET ADDRESS
	DAD D			;OF @(EXPR) AND PUT IT
	POP D			;IN HL
	RET
TV1:	CPI 27			;NOT @, IS IT A TO Z?
	CMC			;IF NOT RETURN C FLAG
	RC
	INX D			;IF A THROUGH Z
        LXI  H,VARBGN-2         ;IF NOT GET ADDRESS
	RLC			;HL->VARIABLE
	ADD L			;RETURN
	MOV L,A			;WITH C FLAG CLEARED
	MVI A,0
	ADC H
	MOV H,A


; *** TSTCH *** TSTNNUM ***
;	
; TSTCH IS USED TO TEST THE NEXT NON-BLANK CHARACTER IN THE TEST
; (POINTED BY DE) AGAINST THE CHARACTER THAT GOLLOWS THE CALL. IF
; THEY DO NOT MATCH, N BYTES OF CODE WILL BE SKIPPED OVER, WHERE N IS
; BETWEEN 0 AND 255 AND IS STORED IN THE SECOND BYTE FOLLOWING THE CALL.
;
; TSTNUM IS USED TO CHECK WHETHER THE TEST (POINTED BY DE) IS A
; NUMBER. IF A NUMBER IS FOUND, B WILL BE NON-ZERO AND HL WILL
; CONTAIN THE VALUE (IN BINARY) OF THE NUMBE, ELSE B AND HL ARE 0
;
TSTCH:  XTHL                    ;*** TSTC ***
	CALL IGNBLK		;IGNORE LEADING BLANKS
	CMP M			;AND TEST THE CHARACTER
	INX H			;COMPARE THE BYTE THAT
        JZ   TC1                ;FOLLOWS THE RST INST.
        PUSH B                  ;WITH THE TEXT (DE->)
        MOV  C,M                ;IF NOT =, ADD THE 2ND
        MVI  B,0                ;BYTE THAT FOLLOWS THE
        DAD  B                  ;RST TO THE OLD PC
        POP  B                  ;I.E., DO A RELATIVE
        DCX  D                  ;JUMP IF NOT =
TC1:    INX  D                  ;IF =, SKIP THOSE BYTES
        INX  H                  ;AND CONTINUE
        XTHL
        RET
;
TSTNUM: LXI  H,0                ;*** TSTNUM ***
        MOV  B,H                ;TEST IF THE TEXT IS
        CALL IGNBLK             ;A NUMBER
TN1:    CPI  '0'                ;IF NOT, RETURN 0 IN
        RC                      ;B AND HL
        CPI  3AH                ;IF NUMBERS, CONVERT
        RNC                     ;TO BINARY IN HL AND
        MVI  A,0F0H             ;SET B TO # OF DIGITS
        ANA  H                  ;IF H>255, THERE IS NO
        JNZ  QHOW               ;ROOM FOR NEXT DIGIT
        INR  B                  ;B COUNTS # OF DIGITS
        PUSH B
        MOV  B,H                ;HL=10*HL+(NEW DIGIT)
        MOV  C,L
        DAD  H                  ;WHERE 10* IS DONE BY
        DAD  H                  ;SHIFT AND ADD
        DAD  B
        DAD  H
        LDAX D                  ;AND (DIGIT) IS FROM
        INX  D                  ;STRIPPING THE ASCII
        ANI  0FH                ;CODE
        ADD  L
        MOV  L,A
        MVI  A,0
        ADC  H
        MOV  H,A
        POP  B
        LDAX D                  ;DO THIS DIGIT AFTER
        JP   TN1                ;DIGIT. S SAYS OVERFLOW
QHOW:   PUSH D                  ;*** ERROR "HOW?" ***
AHOW:   LXI  D,HOW
        JMP  ERROR
;
;*************************************************************
;
; *** MVUP *** MVDOWN *** POPA *** & PUSHA ***
;
; 'MVUP' MOVES A BLOCK UP FROM WHERE DE-> TO WHERE BC-> UNTIL
; DE = HL
;
; 'MVDOWN' MOVES A BLOCK DOWN FROM WHERE DE-> TO WHERE HL->
; UNTIL DE = BC
;
; 'POPA' RESTORES THE 'FOR' LOOP VARIABLE SAVE AREA FROM THE
; STACK
;
; 'PUSHA' STACKS THE 'FOR' LOOP VARIABLE SAVE AREA INTO THE
; STACK
;
MVUP:   CALL COMP               ;*** MVUP ***
        RZ                      ;DE = HL, RETURN
        LDAX D                  ;GET ONE BYTE
        STAX B                  ;MOVE IT
        INX  D                  ;INCREASE BOTH POINTERS
        INX  B
        JMP  MVUP               ;UNTIL DONE
;
MVDOWN: MOV  A,B                ;*** MVDOWN ***
        SUB  D                  ;TEST IF DE = BC
        JNZ  MD1                ;NO, GO MOVE
        MOV  A,C                ;MAYBE, OTHER BYTE?
        SUB  E
        RZ                      ;YES, RETURN
MD1:    DCX  D                  ;ELSE MOVE A BYTE
        DCX  H                  ;BUT FIRST DECREASE
        LDAX D                  ;BOTH POINTERS AND
        MOV  M,A                ;THEN DO IT
        JMP  MVDOWN             ;LOOP BACK
;
POPA:   POP  B                  ;BC = RETURN ADDR.
        POP  H                  ;RESTORE LOPVAR, BUT
        SHLD LOPVAR             ;=0 MEANS NO MORE
        MOV  A,H
        ORA  L
        JZ   PP1                ;YEP, GO RETURN
        POP  H                  ;NOP, RESTORE OTHERS
        SHLD LOPINC
        POP  H
        SHLD LOPLMT
        POP  H
        SHLD LOPLN
        POP  H
        SHLD LOPPT
PP1:    PUSH B                  ;BC = RETURN ADDR.
        RET
;
PUSHA:  LXI  H,STKLMT           ;*** PUSHA ***
        CALL CHGSGN
        POP  B                  ;BC=RETURN ADDRESS
        DAD  SP                 ;IS STACK NEAR THE TOP?
        JNC  QSORRY             ;YES, SORRY FOR THAT
        LHLD LOPVAR             ;ELSE SAVE LOOP VAR'S
        MOV  A,H                ;BUT IF LOPVAR IS 0
        ORA  L                  ;THAT WILL BE ALL
        JZ   PU1
        LHLD LOPPT              ;ELSE, MORE TO SAVE
        PUSH H
        LHLD LOPLN
        PUSH H
        LHLD LOPLMT
        PUSH H
        LHLD LOPINC
        PUSH H
        LHLD LOPVAR
PU1:    PUSH H
        PUSH B                  ;BC = RETURN ADDR.
        RET
LOCR:	LHLD TXTUNF
	DCX H
	DCX H
	RET
;
;*************************************************************
;
; *** PRTSTG *** QTSTG *** PRTNUM *** & PRTLN ***
;
; 'PRTSTG' PRINTS A STRING POINTED BY DE.  IT STOPS PRINTING
; AND RETURNS TO CALLER WHEN EITHER A CR IS PRINTED OR WHEN
; THE NEXT BYTE IS THE SAME AS WHAT WAS IN A (GIVEN BY THE
; CALLER).  OLD A IS STORED IN B, OLD B IS LOST.
;
; 'QTSTG' LOOKS FOR A BACK-ARROW, SINGLE QUOTE, OR DOUBLE
; QUOTE.  IF NONE OF THESE, RETURN TO CALLER.  IF BACK-ARROW,
; OUTPUT A CR WITHOUT A LF.  IF SINGLE OR DOUBLE QUOTE, PRINT
; THE STRING IN THE QUOTE AND DEMANDS A MATCHING UNQUOTE.
; AFTER THE PRINTING THE NEXT 3 BYTES OF THE CALLER IS SKIPPED
; OVER (USUALLY A JUMP INSTRUCTION.
;
; 'PRTNUM' PRINTS THE NUMBER IN HL.  LEADING BLANKS ARE ADDED
; IF NEEDED TO PAD THE NUMBER OF SPACES TO THE NUMBER IN C.
; HOWEVER, IF THE NUMBER OF DIGITS IS LARGER THAN THE # IN
; C, ALL DIGITS ARE PRINTED ANYWAY.  NEGATIVE SIGN IS ALSO
; PRINTED AND COUNTED IN, POSITIVE SIGN IS NOT.
;
; 'PRTLN' PRINTS A SAVED TEXT LINE WITH LINE # AND ALL.
;
PRTSTG: SUB A                  	;*** PRTSTG ***
PS1:	MOV  B,A
PS2:    LDAX D                  ;GET A CHARACTER
        INX  D                  ;BUMP POINTER
        CMP  B                  ;SAME AS OLD A?
        RZ                      ;YES, RETURN
        CALL OUTCH              ;ELSE PRINT IT
        CPI  CR                 ;WAS IT A CR?
        JNZ  PS2                ;NO, NEXT
        RET                     ;YES, RETURN
;
QTSTG:  TSTC '\"',QT3           ;*** QTSTG ***
        MVI  A,'\"'             ;IT IS A "
QT1:    CALL PR1                ;PRINT UNTIL ANOTHER
QT2:	CPI  CR                 ;WAS LAST ONE A CR?
        POP  H                  ;RETURN ADDRESS
        JZ   RUNNXL             ;WAS CR, RUN NEXT LINE
	INX  H                  ;SKIP 3 BYTES ON RETURN
        INX  H
        INX  H
        PCHL                    ;RETURN
QT3:    TSTC '\'',QT4           ;IS IT A '\''
        MVI  A,'\''             ;YES, DO THE SAME
        JMP  QT1                ;AS IN "
QT4:    TSTC '^',QT5            ;IS IT UP-ARROW?
	LDAX d			;YES CONVERT CHARACTER
	XRI 040H		;TO CONTROL-CH
	CALL OUTCH
	INX D
	JMP OT2
QT5:	RET			;NONE OF THE ABOVE
PRTCHS:	MOV A,E
	CMP B
	RZ
	LDAX D
	CALL OUTCH
	INX D
	JMP PRTCHS
;
PRTNUM: MVI  B,0                ;*** PRTNUM ***
        CALL CHKSGN             ;CHECK SIGN
        JP   PN4                ;NO SIGN
        MVI  B,'-'              ;B=SIGN
        DCR  C                  ;'-' TAKES SPACE
PN4:    PUSH D                  ;SAVE
        LXI  D,0AH              ;DECIMAL
        PUSH D                  ;SAVE AS A FLAG
        DCR  C                  ;C=SPACES
        PUSH B                  ;SAVE SIGN & SPACE
PN5:    CALL DIVIDE             ;DIVIDE HL BY 10
        MOV  A,B                ;RESULT 0?
        ORA  C
        JZ   PN6                ;YES, WE GOT ALL
        XTHL                    ;NO, SAVE REMAINDER
        DCR  L                  ;AND COUNT SPACE
        PUSH H                  ;HL IS OLD BC
        MOV  H,B                ;MOVE RESULT TO BC
        MOV  L,C
        JMP  PN5                ;AND DIVIDE BY 10
PN6:    POP  B                  ;WE GOT ALL DIGITS IN
PN7:    DCR  C                  ;THE STACK
        MOV  A,C                ;LOOK AT SPACE COUNT
        ORA  A
        JM   PN8                ;NO LEADING BLANKS
        MVI  A,' '              ;LEADING BLANKS
        CALL OUTCH
        JMP  PN7                ;MORE?
PN8:    MOV  A,B                ;PRINT SIGN
        ORA  A
	CNZ OUTCH		;MAYBE CR NULL
        MOV  E,L                ;LAST REMAINDER IN E
PN9:    MOV  A,E                ;CHECK DIGIT IN E
        CPI  0AH                ;10 IS FLAG FOR NO MORE
        POP  D
        RZ                      ;IF SO, RETURN
        ADI  '0'                ;ELSE CONVERT TO ASCII
        CALL OUTCH              ;AND PRINT THE DIGIT
        JMP  PN9                ;GO BACK FOR MORE
;
PRTLN:  LDAX D                  ;*** PRTLN ***
        MOV  L,A                ;LOW ORDER LINE #
        INX  D
        LDAX D                  ;HIGH ORDER
        MOV  H,A
        INX  D
        MVI  C,4H               ;PRINT 4 DIGIT LINE #
        CALL PRTNUM
        MVI  A,' '              ;FOLLOWED BY A BLANK
        CALL OUTCH
	RET
;
; *** COMMAND TABLES ***
TAB1:	ITEM 'LIST',LIST	;DIRECT COMMANDS
	ITEM 'NEW',NEW
	ITEM 'RUN',RUN
TAB2:	ITEM 'NEXT',NEXT	;DIRECT/STATEMENT
	ITEM 'LET',LET
	ITEM 'IF',IFF
	ITEM 'GOTO',GOTO
	ITEM 'GOSUB',GOSUB
	ITEM 'RETURN',RETURN
	ITEM 'NEW',NEW
	ITEM 'FOR',FOR
	ITEM 'INPUT',INPUT
	ITEM 'PRINT',PRINT
	ITEM 'STOP',STOP
	ITEM ,NOREC
NOREC:	JMP DEFLT
;*** JMP USER-COMMAND ***
TAB3:	ITEM 'RND',RND
	ITEM 'ABS',ABS
	ITEM 'SIZE',SIZE
	ITEM ,NOREF
NOREF:	JMP NOTF
; *** JMP USER-FUNCTION ***
TAB4:	ITEM 'TO',FR1
	ITEM ,QWHAT
TAB5:	ITEM 'STEP',FR2
	ITEM ,FR3
;*** RELATION OPERATORS	***
TAB6:	ITEM '>=',XPR1
	ITEM '#',XPR2
	ITEM '>',XPR3
	ITEM '=',XPR5
	ITEM '<=',XPR4
	ITEM '<',XPR6
	ITEM ,XPR7
RANEND:	EQU *
;
;*** INPUT OUTPOUT ROUTINES ***
; USER MUST VERIFY AND/OR MODIFY THESE ROUTINES
;
; *** CRLF *** OUTCH ***
; CRLF WILL OUTPUT A CR. ONLY A & FLAGS MAY CHANGE ON RETURN
; OUTCH WILL OUTPUT THE CHARACTER IN A. IF THE CHARACTER IS CR, IT
; WILL ALSO OUTPUT A L AND THREE NULLS. FLAGS MAY CHANGE AT RETURN.
; OTHER REGISTERS DO NOT.
; *** CHKIO ***   GETLN ***
;
; CHKIO CHECKS TO SEE IF THERE IS ANY INPUT. IF NO INPUT IT RETURNS
; WITH Z FLAG. IF THERE IS INPUT, IT FURTHER CHECKS WHETHER INPUT IS
; CONTROL-C. IF NOT CONTROL-C IT RETURNS THE CHARACTER IN A WITH Z
; FLAG CLEARED. IF INPUT IS CONTROL-C CHKIO JUMPS TO 'INIT' AND WILL
; NOT RETURN. ONLY A & FLAGS MAY CHANGE AT RETURN.
;
; GETLN READS A INPUT LINE INTO 'BUFFER' ; IT FIRST PROMPT THE
; CHARACTER IN A (GIVEN BY THE CALER), THEN IT FILLS THE BUFFER
; AND ECHOS. BACK-SPACE IS USED TO DELETE THE LAST CHARACTER (IF THER
; IS ONE). CR SIGNALS THE END OF THE LINE AND CAUSE 'GETLN' TO RETURN
;
CRLF:	MVI A,00DH		;CR IN A
OUTCH:	JMP _OUT_
CHKIO:	JMP _IN_
GETLN:  LXI  D,BUFFER           ;PROMPT AND INIT.
GL1:	CALL OUTCH
GL2:	CALL CHKIO              ;CHECK KEYBOARD
        JZ   GL1                ;NO INPUT, WAIT
        CPI  7FH                ;DELETE LAST CHARACTER?
        JZ   GL2                ;YES
GL3:	STAX D			;SAVE CH
	CPI 008H		;IS IT BACK-SPACE?
	JNZ GL4			;NO MORE TESTS
	MOV A,E			;YES DELETE
	CPI BUFFER AND OFFH
	JZ GL2			;NOTHING TO DELETE
	LDAX D
	DCX D
	JMP GL1
GL4:	CPI CR			;WAS IT CR?
	JZ GL5			;YES END OF LINE
	MOV A,E			;ELSE MORE ROOM?
	CPI BUFEND AND OFFH
	JZ GL2			;NO WAIT FOR CR/RUB-OUT
	LDAX D			;YES BUMP POINTER
	INX D
	JMP GL1
GL5:	INX D			;END OF LINE
	INX D			;BUMP POINTER
	MVI A,0FFH		;PUT A MARKER AFTER IT
	STAX D
	DCX D
	JMP CRLF
_OUT_:	PUSH PSW		;OUTPUT ROUTINE
OT1:	IN 0			;PRINT WHAT IS IN A
	ANI 001H		;TBE BIT
	JZ 0T1			;WAIT UNTIL READY
	POP PSW
	OUT 1
	CPI CR			;WAS IT CR?
	RNZ			;NO, RETURN
	MVI A,LF		;YES GIVE LF
	CALL _OUT_
	MVI A,CR
	RET
_IN_:	IN 0
	ANI 002H		;DAV BIT
	RZ
	IN 1			;CHECK INPUT
	ANI 07FH
	CPI 003H		;IS IT CONTROL-C?
	RNZ			;NO, RETURN CH
	JMP INIT		;YES RESTART

	END
