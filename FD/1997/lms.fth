\ lms.fth    An implementation an LMS adaptive filter
\
\ This is an ANS Forth program requiring:
\         1. The Floating point word set
\         2. The Forth Scientific Library Array words
\
\   There is an environmental dependency in that it is assumed
\   that the float stack is separate from the parameter stack

\ This code is released to the public domain  September 1996
\ Taygeta Scientific Inc.
\
\ $Author:   skip  $
\ $Workfile:   lms.fth  $
\ $Revision:   1.0  $
\ $Date:   17 Jul 1997 02:57:08  $
\
\ =================================================================

\ the Array words from the Forth Scientific Library
S" /usr/local/lib/forth/fsl-util.fth" INCLUDED

\ =================================================================

8 CONSTANT N       \ the filter order
8 CONSTANT M       \ the running mean block size

FVARIABLE beta   \ weighting coefficient

M N MAX CONSTANT Ny

M Float Array Err{      \ filter error output
Ny Float Array Y{       \ filtered data
N Float Array A{        \ filter coefficients
N Float Array X{        \ input data


: F+! ( addr -- , F: x -- )  \ increment a Float variable
    DUP F@ F+ F!
;

: initialize ( -- , F: beta -- )  \ one time initialization

    2.0E0 F* beta F!

    M 0 DO
        0.0E0 Err{ I } F!
    LOOP

    N 0 DO
	0.0E0 A{ I } F!
        0.0E0 X{ I } F!
    LOOP

    Ny 0 DO
	0.0E0 Y{ I } F!
    LOOP
;

: }shuffle ( n x{ -- )   \ slide all the data values down by one

    1 ROT 1- DO
             DUP I 1- } F@ DUP I } F!
    -1 +LOOP

    DROP
;

: do_filter ( -- , F: x --  )  \ apply the LMS filter on the
                               \ current data

    N X{ }shuffle
    FDUP X{ 0 } F!

    0.0E0
    N 0 DO
	A{ N 1- I - } F@ X{ I } F@ F* F+
    LOOP

    Ny Y{ }shuffle
    FDUP Y{ 0 } F!

    FNEGATE F+       \ err
    N Err{ }shuffle
    Err{ 0 } F!

;

: get_adjustment ( k -- , F: -- x )    \ calculate adjustement

    NEGATE
    0.0E0
    M 0 DO
	  Err{ OVER I + } F@ X{ OVER I + } F@ F* F+
    LOOP

    DROP
    Beta F@ F* M S>D D>F F/
;

: lms_adapt ( -- )  \ apply LMS adaptive scheme


    N 0 DO
	I get_adjustment
        A{ I } F+!
    LOOP

;


: lms_filter ( -- , F: y -- yf )  \ do LMS filter for one data point

    do_filter

    lms_adapt

    Y{ 0 } F@ Err{ 0 } F@ F-
;

\ =================================================================


 0.85E0 initialize


\ =================================================================






