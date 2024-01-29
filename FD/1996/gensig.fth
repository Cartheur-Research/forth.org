\
\ gensig.fth   Generates a reference chirp test signal
\              or, if SAMPLING? is true, generate a sampled signal

\ This is an ANS Forth program requiring:
\         1. The Floating point word set
\         2. The conditional compilation words in the
\            PROGRAMMING-TOOLS wordset
\   There is an environmental dependency in that it is assumed
\   that the float stack is separate from the parameter stack

\ This code is released to the public domain  September 1996
\ Taygeta Scientific Inc.

\ $Author:   skip  $
\ $Workfile:   gensig.fth  $
\ $Revision:   1.0  $
\ $Date:   28 Sep 1996 20:04:22  $
\ ================================================================

FALSE CONSTANT SAMPLING?

10.0E0 FCONSTANT F_MIN           \ 10 "Hz" minimum frequency
12.0E0 FCONSTANT F_MAX           \ 12 "Hz" maximum frequency

4.50E0 FCONSTANT SWEEP           \ the frequency sweep time

FVARIABLE DF

6.283185307E0 FCONSTANT TWO_PI

0.0078125E0 FCONSTANT DT            \ 128 "Hz" sample rate

: tone ( -- , F: t f -- x )
    F* TWO_PI F* FSIN
;

: S>F ( x -- , F: -- fx )
    S>D D>F
;

SAMPLING? [IF]


\ 16 CONSTANT DECIMATE                \ effective sampling at 8 Hz
20 CONSTANT DECIMATE                \ effective sampling at 6.4 Hz

F_MAX F_MIN F+ 2.0E0 F/ FCONSTANT F_CENTER

: MIX ( -- , F: x t -- x' )

    F_CENTER F* TWO_PI F* FCOS
    F*

    \ account for loss due to the mixer shifting stuff to
    \ both a low and high band
    2.0E0 F*

;

 0.01279E0 FCONSTANT COEF_A
-1.65561E0 FCONSTANT COEF_B
 0.70676E0 FCONSTANT COEF_C

FVARIABLE IN_0     0.0E0 IN_0 F!
FVARIABLE IN_1     0.0E0 IN_1 F!
FVARIABLE IN_2     0.0E0 IN_2 F!
FVARIABLE OUT_0    0.0E0 OUT_0 F!
FVARIABLE OUT_1    0.0E0 OUT_1 F!
FVARIABLE OUT_2    0.0E0 OUT_2 F!

\ first order low-pass Butterworth filter
: FILTER ( -- , F: x -- x' )

    IN_0 F!

    IN_0 F@ IN_2  F@ F+
    IN_1 F@ 2.0E0 F* F+
    COEF_A F*

    OUT_1 F@ COEF_B F* F-
    OUT_2 F@ COEF_C F* F-

    FDUP OUT_0 F!

    \ shift data for next time

    IN_1 F@ IN_2 F!
    IN_0 F@ IN_1 F!

    OUT_1 F@ OUT_2 F!
    OUT_0 F@ OUT_1 F!

;

[ELSE]

1 CONSTANT DECIMATE

: MIX ( -- , F: x t -- x )
    FDROP                  \ do nothing for reference signal
;

: FILTER ( -- , F: x -- x )  ; IMMEDIATE

[THEN]




: freq ( -- , F: t -- f )     \ calculate the frequency for this time

    FDUP       F0< IF FDROP F_MIN EXIT THEN
    SWEEP FOVER F< IF FDROP F_MAX EXIT THEN

    SWEEP F/ DF F@ F* F_MIN F+
;

: gensig ( -- , F: maxt -- )

    DT F/ F>D DROP

    F_MAX F_MIN F- DF F!

    CR

    0.0E0      \ the time
    0 DO
	FDUP FDUP freq
        tone
	FOVER MIX FILTER

	I DECIMATE MOD 0= IF
	                    FOVER F. F. CR
			  ELSE
			    FDROP
			  THEN
		      
		    
        DT F+
    LOOP

    FDROP
;


SWEEP gensig bye

