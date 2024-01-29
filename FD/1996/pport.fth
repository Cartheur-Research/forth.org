\ pport.fth              Parallel Port input for the PC

\ This is an ANS Forth program for manipulating the PC
\ parallel port under MSDOS or Linux requiring:
\         1. The File Access word set
\         2. the conditional compilation words in the
\             PROGRAMMING-TOOLS word set
\         3. For use under MSDOS the word
\             : MSDOS  ;
\            must be defined before loading this file
\         4. It is assumed that the inverse of COUNT is DROP 1-

\ This code is released to the public domain      July 1996
\ Taygeta Scientific Inc.

\ $Author:   skip  $
\ $Workfile:   pport.fth  $
\ $Revision:   1.1  $
\ $Date:   13 Jul 1996 02:35:08  $

\ ===================================================================

\ adapted from the Forth Scientific Library
\ assumes that the inverse of COUNT is DROP 1-
\ (this assumption could be avoided if C" was allowed to
\  be interpreted, but only the FILE S" is)

: DEFINED   ( c-addr u -- t/f )    \ returns definition status of
      DROP 1- FIND SWAP DROP       \ a word, true if its there
;



\ ===================================================================



S" MSDOS" DEFINED [IF]

S" fcontrol.seq" INCLUDED         \ from FD XVII/2


: initialize ( -- )    ; IMMEDIATE         \ nothing to do here
: close-port ( -- )    ; IMMEDIATE

#PORT       CONSTANT #DATA

[ELSE]           \ assume Unix/Linux

S" /usr/local/lib/forth/ports.fth" INCLUDED

: initialize ( -- )      \ open up the parallel I/O port
    init-port TO #IOPORTS
;

HEX

378       CONSTANT #DATA                  \ set as appropriate

[THEN]


\ ===================================================================


#DATA 1+  CONSTANT #STATUS
#DATA 2 + CONSTANT #COMMAND


DECIMAL

27 CONSTANT ESC               \ the escape character

: .binary ( n -- )          \ display in binary
    BASE @ SWAP
    2 BASE !
    8 U.R
    BASE !
;

: .hex ( n -- )          \ display in hex
    BASE @ SWAP
    HEX
    4 U.R
    BASE !
;


\ read specified port, repeating at each keystroke until ESC
: test_input ( n -- )

    initialize

    CR

    BEGIN
	DUP pc@ .binary CR
	KEY ESC =
    UNTIL

    DROP
    close-port

;

\ ===================================================================


: test_status ( -- )

    CR ." Reading #STATUS port at " #STATUS .hex

    #STATUS test_input

;



: test_command ( -- )

    CR ." Reading #COMMAND port at " #COMMAND .hex

    #COMMAND test_input

;





