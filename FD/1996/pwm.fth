\ pwm.fth           Simple PWM of a parallel port pin
\                   for controlling a DC motor.


\ $Author:   skip  $
\ $Workfile:   pwm.fth  $
\ $Revision:   1.1  $
\ $Date:   28 May 1996 02:38:24  $

\ This code is released to the public domain. Everett Carter, May 1996

\ code to set up the parallel port
S" /usr/local/lib/forth/fcontrol.fth" INCLUDED


\ ===============================================================

\ We will just assume that bit 0 is the motor,
\ but by playing games with the following masks we could
\ control multiple motors

0 VALUE off-mask
1 VALUE on-mask

\ ====================== Timing Control ============================


VARIABLE hitime               \ 0..cycle-time, the motor will stall
                              \ for very small values (like 4 or 5),
                              \ depending upon the motor

VARIABLE cycle-time           \ try something like 100, depends upon
                              \ the motor and the CPU




\ The ANS MS is too coarse, we want the WHOLE CYCLE TIME to be
\ on the order of 1 ms, so we are falling back to the old standby
\ idle loop.  The values used here may need tuning for different
\ motors and CPUs

100 VALUE delay-time        \ cycle time granularity

: delay delay-time 0 DO LOOP ;


\ ===============================================================

: verify ( -- )       \ make sure we have useful settings

    hitime     @ 0 <  ABORT" illegal hitime (< 0)"
    cycle-time @ 1 <  ABORT" illegal cycle-time (< 1)"

    hitime @ cycle-time @ > ABORT" hitime > cycle-time"
;


: init ( -- )
    verify
    init-port TO #PORT
;

: run_motor ( -- )      \ run motor until keystroke received

    BEGIN
        hitime @ 0 > IF on-mask #PORT pc! THEN

        cycle-time @ 0 DO
	    I hitime @ = IF off-mask #PORT pc! ELSE
	                    KEY? IF KEY DROP UNLOOP EXIT THEN
	                 THEN
	    delay
        LOOP

     AGAIN


;

\ ===============================================================

: run ( -- )         \ initialize, run, and clean-up
 
    init

    run_motor

    off-mask #PORT pc!

    #PORT CLOSE-FILE DROP
;





