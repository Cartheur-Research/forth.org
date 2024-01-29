\ rls.fth    An implementation of the square root form of
\            an RLS adaptive filter
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
\ $Workfile:   rls.fth  $
\ $Revision:   1.1  $
\ $Date:   16 Jul 1997 23:19:58  $
\
\ =================================================================

\ the Array words from the Forth Scientific Library
S" /usr/local/lib/forth/fsl-util.fth" INCLUDED

\ =================================================================

8 CONSTANT N       \ the filter order

FVARIABLE sigma    \ square root of the initial data variance
FVARIABLE lambda   \ weighting coefficient

FVARIABLE Err       \ filter error output
FVARIABLE Gain      \ filter gain

N Float Array K{        \ filter gain components
N Float Array Phi{      \ filter data
N Float Array H{        \ filter coefficients

\ internal filter coefficient data
FVARIABLE y_old
N Float Array F{
N Float Array V{
N Float Array Alpha{

\ U{ and D{ are actually diagonal matricies such that the
\ filter covariance is U * D * Transpose( U )
N Float Array U{
N Float Array D{

: F+! ( addr -- , F: x -- )  \ increment a Float variable
    DUP F@ F+ F!
;

: initialize ( -- , F: sigma lambda -- )  \ one time initialization

    lambda F!

    N 0 DO
        0.0E0 Phi{ I } F!
        0.0E0 H{ I } F!
	1.0E0 U{ I } F!
        FDUP D{ I } F!
    LOOP

    sigma F!

;

: }shuffle ( x{ -- )   \ slide all the data values down by one

    1 N 1- DO
             DUP I 1- } F@ DUP I } F!
    -1 +LOOP

    DROP
;

: do_filter ( -- , F: yn --  )  \ apply the RLS filter on the
                                \ current data

    0.0E0
    N 0 DO
	Phi{ I } F@ H{ N 1- I - } F@ F* F+
    LOOP

    FNEGATE FOVER F+       \ err
    Err F!

    y_old F!

;

: preset ( -- )    \ initial filter setup for each step

    N 0 DO
	    U{ I } F@ Phi{ I } F@ F*
	    F{ I } F!

	    D{ I } F@ F{ I } F@ F* V{ I } F!
    LOOP

    V{ 0 } F@ K{ 0 } F!

    V{ 0 } F@ F{ 0 } F@ F*  lambda F@ F+      ( -- , F: alpha )

    D{ 0 } F@ FOVER F/ D{ 0 } F!         \ d{0} = d{0}/alpha

    Alpha{ 0 } F!

;

: adjust_gain ( -- )  \ apply RLS adaptive scheme to adjust the gain

    preset


    N 1 DO
	\ d{i} = d{i}*alpha/lambda
	D{ I } F@ Alpha{ I 1- } F@ F* lambda F@ F/ D{ I } F!

	\ calculate new alpha
	V{ I } F@ F{ I } F@ F* Alpha{ I 1- } F@ F+
        FDUP Alpha{ I } F!

        \ finish update of D,  d{i} = d{i} / new alpha
	D{ I } F@ FSWAP F/ D{ I } F!

        \ update U keeping a copy of the old value
	F{ I } F@ Alpha{ I 1- } F@ F/ FNEGATE
	K{ I 1- } F@ F* U{ I } F@
        FSWAP FOVER F+ U{ I } F!        ( -- , F: uold )

        \ update the gain
	V{ I } F@ F* K{ I 1- } F@ F+
        K{ I } F!
    LOOP

    \ compete the Gain update
    N 0 DO
           K{ I } F@ Alpha{ I } F@ F/  K{ I } F!
       LOOP
   
;

: adjust_coefficients ( -- )

    Err F@

    N 0 DO
        FDUP K{ I } F@ F*
	H{ I } F+!
    LOOP

    FDROP

    Phi{ }shuffle
    y_old F@ Phi{ 0 } F!

;


: rls_filter ( -- , F: y -- yf )  \ do RLS filter for one data point

    do_filter

    adjust_gain

    adjust_coefficients

    Err F@
;

\ =================================================================


0.65E0 0.5E0 initialize

\ =================================================================
