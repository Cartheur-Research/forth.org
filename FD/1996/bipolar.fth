\ bipolar.fth         Driving a Bipolar stepper motor with a
\                     dual H-bridge

\ note: much of this code closely duplicates the code from the
\       Jan/Feb 1996 listing: steppers.seq.  The primary differences
\       are the values of the sequence array and that it is assumed
\       that the pins drive a dual H-bridge like Figure 3.


\ $Author:   skip  $
\ $Workfile:   bipolar.fth  $
\ $Revision:   1.1  $
\ $Date:   28 May 1996 02:42:46  $

\ This code is released to the public domain. Everett Carter, May 1996

\ code to set up the parallel port
S" /usr/local/lib/forth/fcontrol.fth" INCLUDED
S" /usr/local/lib/forth/fsl-util.fth" INCLUDED
S" /usr/local/lib/forth/structs.fth"  INCLUDED

\ ===============================================================


\ We will just assume that bits 0 and 1 drive the motor coil 1,
\ and bits 2 and 3 drive motor coil 2


structure: sequence
    integer: .n
    integer: .index
    4 integer array: .s{
;structure

sequence bipolar

: init-seq ( -- )

    4 bipolar .n !
    0 bipolar .index !
    2 bipolar .s{ 0 } !
    4 bipolar .s{ 1 } !
    1 bipolar .s{ 2 } !
    8 bipolar .s{ 3 } !

;

-1 VALUE direction?

12 VALUE wtime


: idx++ ( seq_hdl -- idx )     \ increment the index, return old value
    2DUP .n @ >R
         .index DUP @
    DUP 1+ R> MOD
    ROT !
;


: idx-- ( seq_hdl -- idx )     \ decrement the index, return old value
    2DUP .n @ >R
         .index DUP @
    DUP 1- R> OVER
    0 < IF 1- SWAP THEN DROP
    ROT !
;


: fsteps ( seq_hdl n -- )

    0 DO wtime MS
	2DUP idx++ >R
	2DUP .s{ R> } @ #PORT pc!
    LOOP

    2DROP
;

: rsteps ( seq_hdl n -- )

    0 DO wtime MS
	2DUP idx-- >R
	2DUP .s{ R> } @ #PORT pc!
    LOOP

    2DROP
;


: reverse ( -- )                \ toggles rotation direction
    direction? IF 0 ELSE -1 THEN
	TO direction?
;

: steps ( n seq_hdl -- )
    ROT
    direction? IF fsteps ELSE rsteps THEN
;

\ =================================================================


init-seq


\ typical usage:    12 bipolar steps

