
-->  \ Extended Memory Access      Screen 0 of 6      19JAN84RES
Most versions of FORTH live in a 64 Kbyte segment of memory --
dictionary, stacks, disk buffers, etc., are confined to one such
segment. Furthermore, some versions have no words to even refer
to other parts of memory, limiting all address references to 16
bits ( one stack item ).
The following six screens define "extended memory access" words
for transfer of data to/from any part of 1 Mbyte of memory, in
the address range  00000. - FFFFF.
@_  ( da -- n)  fetches a 16-bit word from da, eg, ABCDE. @_
!_  ( n da --)  stores  "    "   "    at da, eg, 1234 ABCDE. !_
C@_ ( da -- c)  fetches an 8-bit byte from da, eg, ABCDE. C@_
C!_ ( c da --)  stores  "    "   "    at da, eg, 12 ABCDE. C!_
DUMP_ ( da n --)  displays n bytes of memory beginning at da

               -- Bob Sawyer,  Torrance CA --
\ Extended Memory Access      Screen 1 of 6           19JAN84RES

HEX    ASSEMBLER
                                \ set up new  seg:offset using
CREATE  NEW-SEG                 \ 20-bit address ZYXWV in AX DX:
       ( AX DX -- DX DI)        \    000Z in AX, YXWV in DX
        AX, # 1         ROR     \ rotate 000Z to Z000
        AX, # 1         ROR
        AX, # 1         ROR
        AX, # 1         ROR
        DX              PUSH    \ move YXWV from DX to DI
        DI              POP
        DX, AX          MOV     \ move Z000 into DX
                        RET     \ new  seg:offset  ( Z000:YXWV )
                       -->      \    is in  DX:DI

\ Extended Memory Access        Screen 2 of 6         19JAN84RES

CODE @_  ( da -- n)             \ 20-bit addr on stack as double
                                \   word; eg,  B8000.  @_   will
                                \   fetch the 16 bits at B8000.
        AX              POP     \ put hi word in AX
        DX              POP     \ put lo word in DX
        NEW-SEG         CALL    \ new   seg:offset   is DX:DI
        AX, DS          MOV     \ save current segment in AX
        DS, DX          MOV     \ load new segment
        CX, [DI]        MOV     \ fetch data from seg:offset
        CX              PUSH    \ put data on stack
        DS, AX          MOV     \ restore old segment
        NEXT            JMP
END-CODE                -->

\ Extended Memory Access        Screen 3 of 6         19JAN84RES

CODE !_ ( n da)                 \ eg.,  10A3 B8C65. !_  will
                                \  move value 10A3 to B8C65.
        AX              POP
        DX              POP
        NEW-SEG         CALL
        CX, DS          MOV
        DS, DX          MOV
        BX              POP
        [DI], BX        MOV
        DS, CX          MOV
        NEXT            JMP
END-CODE                -->


\ Extended Memory Access        Screen 4 of 6         19JAN84RES

CODE C@_  ( da -- c)            \ 20-bit addr on stack as double
                                \   word; eg,  B8000.  C@_  will
                                \   fetch the byte at B8000.
        AX              POP     \ put hi word in AX
        DX              POP     \ put lo word in DX
        NEW-SEG         CALL    \ new   seg:offset   is DX:DI
        CX, DS          MOV     \ save current segment in AX
        DS, DX          MOV     \ load new segment
        AX, AX          SUB     \ clear AX
        AL, [DI]        MOV     \ fetch data from seg:offset
        AX              PUSH    \ put data on stack
        DS, CX          MOV     \ restore old segment
        NEXT            JMP
END-CODE                -->
\ Extended Memory Access        Screen 5 of 6         19JAN84RES

CODE C!_ ( c da)                \ eg.,   A3 B8C65. C!_  will
                                \  move value A3 to B8C65.
        AX              POP
        DX              POP
        NEW-SEG         CALL
        CX, DS          MOV
        DS, DX          MOV
        BX              POP
        [DI], BL        MOV
        DS, CX          MOV
        NEXT            JMP
END-CODE                -->


\ Extended memory dump (byte/ASCII)   Screen 6 of 6   19JAN84RES
HEX
: ?PRINTABLE ( c -- f)  DUP 19 > SWAP 7F < AND ;
: DISP ( da --)  \ display 16 bytes beginning at given addr
        10 0 DO 2DUP I DUP 8 = 2* SPACES 1 U* D+ C@_ 2 U.R
                 SPACE LOOP  2 SPACES
        10 0 DO 2DUP I 1 U* D+ C@_
            DUP ?PRINTABLE NOT IF DROP 0FA THEN EMIT LOOP
        2DROP CR ;
: HEAD  ( da) DROP ." Address" 4 SPACES 10 0 DO DUP 0F AND 1 U.R
    2 SPACES  I 7 = 2* SPACES 1+ LOOP  SPACE 10 0 DO DUP 0F AND
    1 U.R 1+ LOOP  DROP CR ;
: DUMP_  ( da n --) \ dump any part of memory 16 bytes per row
    CR  CR  BASE @  >R  HEX  ROT ROT 2DUP HEAD CR ROT 0 DO 2DUP
            I 1 U* D+ 2DUP 6 D.R 4 SPACES  DISP 10 +LOOP
        2DROP  R>  BASE ! ;                     CR ." HEX " CR


