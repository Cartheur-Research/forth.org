\
\ regen.fth   reconstructs the original signal from
\             the sampled signal
\
\ This is an ANS Forth program requiring:
\         1. The Floating point word set
\         2. The File wordset
\         3. The conditional compilation words in the
\            PROGRAMMING-TOOLS wordset
\         4. The Forth Scientific Library Array words
\         5. The Forth Scientific Library ASCII file I/O words
\   There is an environmental dependency in that it is assumed
\   that the float stack is separate from the parameter stack

\ This code is released to the public domain  September 1996
\ Taygeta Scientific Inc.

\ $Author:   skip  $
\ $Workfile:   regen.fth  $
\ $Revision:   1.0  $
\ $Date:   28 Sep 1996 20:04:50  $
\ ================================================================

S" /usr/local/lib/forth/fsl-util.fth" INCLUDED
S" /usr/local/lib/forth/fileio.fth"   INCLUDED

FALSE CONSTANT STANDALONE

-1 VALUE fin  \ input file handle
-1 VALUE fout \ output file handle


0.0078125E0 FCONSTANT OUT_DT          \ 128 "Hz" output rate
0.15625E0 FCONSTANT DT              \ 6.4 "Hz" sample rate


3.1415926536E0 FCONSTANT PI
6.283185307E0  FCONSTANT TWO_PI

29  CONSTANT NUM_SAMPLES
560 CONSTANT NUM_OUTPUT

NUM_SAMPLES FLOAT ARRAY t{
NUM_SAMPLES FLOAT ARRAY x{               \ the samples

CREATE outbuf 32 ALLOT
CREATE CRLF   2 ALLOT

FVARIABLE PI/T

9 CONSTANT TAB_CHAR       \ the TAB character
CREATE TAB 1 ALLOT


STANDALONE [IF]

variable f_index   1 f_index !


: next_file ( -- c-addr u )

    f_index @ argc >= if
        0 0 
    else
        f_index @ argv
        1 f_index +!
    then

;


[ELSE]

: next_file ( -- c-addr u )
    bl word count
;

[THEN]


: S>F ( x -- , F: -- fx )
    S>D D>F
;

: }zero ( n x -- )
    SWAP 0 DO
	     DUP I } 0.0E0 F!
    LOOP

    DROP
;

: sinc ( -- , F: x -- sincx )

    FDUP F0= IF   FDROP 1.0E0
             ELSE FDUP FSIN FSWAP F/
             THEN
;

: estimate ( n -- , F: t -- x )

    PI DT F/ PI/T F!

    0.0E0 FSWAP  ( F: sum t )
    0 DO
        I S>F DT F*
	FOVER FSWAP F-    ( F: sum t t-nT )
        PI/T F@ F*
        sinc
	X{ I } F@ F*
	FROT F+    
        FSWAP    
    LOOP

    FDROP
;


: print_endline ( -- )
    CRLF
    1              \ for MS-DOS use 2 instead of 1
    fout write-token
;

: print_tab ( -- )
    TAB
    1
    fout write-token
;

: regen ( --<infile outfile>-- )

    10 CRLF   C!  13  CRLF 1+ C!
    TAB_CHAR TAB C!

    next_file

    R/O OPEN-FILE ABORT" unable to open data file"
    TO fin

    next_file
    \ open the output file
    W/O CREATE-FILE ABORT" unable to open output file" TO fout
    
    CR

    NUM_SAMPLES x{ }zero
    NUM_SAMPLES t{ }zero

    NUM_SAMPLES 0 DO
        I . 
        fin get-float  FDUP F. t{ I } F!
	fin get-float  FDUP F. x{ I } F!
        CR
    LOOP


    fin CLOSE-FILE DROP

    NUM_OUTPUT 0 DO
        I S>F OUT_DT F* FDUP outbuf fout write-float
	print_tab
    
	NUM_SAMPLES  estimate
	outbuf fout write-float
        print_endline
    LOOP


    fout CLOSE-FILE DROP

;










