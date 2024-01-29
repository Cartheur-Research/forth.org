\
\ lsq.fth   Calculates the Least-Squares optimal fit to a
\           straight line, y = ax + b, from  the sampled x,y data.
\           Presumes that all the measurement uncertainty is in the y
\
\           The input file consists of a single line giving
\           the number of data points, followed by that many
\           lines of x y sample points.
\
\
\ This is an ANS Forth program requiring:
\         1. The Floating point word set
\         2. The File wordset
\         3. The conditional compilation words in the
\            PROGRAMMING-TOOLS wordset
\         4. The Forth Scientific Library ASCII file I/O words
\         5. The standalone version requires access to the command
\            line arguments, the PFE version is implemented here
\   There is an environmental dependency in that it is assumed
\   that the float stack is separate from the parameter stack

\ This code is released to the public domain  August 1997
\ Taygeta Scientific Inc.

\ $Author:   skip  $
\ $Workfile:   lsq.fth  $
\ $Revision:   1.0  $
\ $Date:   27 Aug 1997 12:17:16  $
\ ================================================================

S" /usr/local/lib/forth/fsl-util.fth" INCLUDED
S" /usr/local/lib/forth/fileio.fth"   INCLUDED

FALSE CONSTANT STANDALONE    \ set true to run as a standalone script

-1 VALUE fin  \ input file handle


VARIABLE n      \ count of data points
FVARIABLE sumx
FVARIABLE sumxz
FVARIABLE sumz
FVARIABLE sumx2

STANDALONE [IF]

VARIABLE f_index   1 f_index !


: next_file ( -- c-addr u )  \ get file name from the command line
                             \ when running as a standalone script
                             \ this version is for PFE
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

: F+! ( addr -- , F: x -- )    \ +! for floats
                               \ presumes separate float stack
     DUP F@ F+ F!
;

\ =================================================================

: lsq-init ( -- )
    0 n !
    0.0E0 sumx  F!
    0.0E0 sumxz F!
    0.0E0 sumx2 F!
    0.0E0 sumz  F!
;

: calc-det ( -- , F: -- d )

    n @ S>F  sumx2 F@ F*
    sumx F@ FDUP F* F-
;

: estimate ( -- , F: -- b a )

    calc-det

    \ calculate b
    sumx2 F@ sumz F@ F* sumx F@ sumxz F@ F* F- FOVER F/

    FSWAP
    \ now calculate a
    n @ S>F sumxz F@ F*  sumx F@ sumz F@ F* F- FSWAP F/
;

: lsq ( --<infile>-- )


    lsq-init

    next_file

    R/O OPEN-FILE ABORT" unable to open input data file"
    TO fin

    CR

    fin get-int  DUP n !       \ read count of points        

    0 DO
        I . 
	fin get-float  \ get X point
        FDUP F.
	FDUP sumx F+!
	FDUP FDUP F* sumx2 F+!
    
	fin get-float  \ get Z point
        FDUP F.
	FDUP sumz  F+!
        F*   sumxz F+!    
        CR
    LOOP


    fin CLOSE-FILE DROP

    estimate

    ." slope (a): " F.
    ." intercept (b): " F. CR

;










