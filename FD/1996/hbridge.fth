\ hbridge.fth         Driving a DC motor with an H-bridge


\ $Author:   skip  $
\ $Workfile:   hbridge.fth  $
\ $Revision:   1.1  $
\ $Date:   28 May 1996 02:40:22  $

\ This code is released to the public domain. Everett Carter, May 1996

\ code to set up the parallel port
S" pwm.fth" INCLUDED


\ ===============================================================


\ We will just assume that bits 0 and 1 drive the motor,
\ but by playing games with the following masks we could
\ control multiple motors

: forward
    1 TO on-mask
;

: reverse
    2 TO on-mask
;

\ usage:  forward (or reverse) run
