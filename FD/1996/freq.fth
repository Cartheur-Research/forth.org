\ freq.fth   Simple implementations of a period and frequency counter
\            sampling data on the parallel #STATUS port bit 7

\ This code is released to the public domain  September 1996
\ Taygeta Scientific Inc.

\ $Author:   skip  $
\ $Workfile:   freq.fth  $
\ $Revision:   1.0  $
\ $Date:   28 Sep 1996 20:05:14  $
\ ================================================================

HEX

378       CONSTANT #DATA
#DATA 1+  CONSTANT #STATUS
#DATA 2 + CONSTANT #COMMAND


DECIMAL


\ ================================================================
\ the following two words are adapted from:
\ Hendtlass, T., 1993; Real Time Forth
\ if you don't already have it, GET THIS BOOK!
\ contact: tim@brain.physics.swin.oz.au

\ An F-PC specific <read_clock>
code <read_clock> ( -- n )
    push ax
    mov ax, # 0
    int 26
    pop ax
    push dx
    next
end-code

: DOWN-COUNTER        \ creates a countdown timer
    CREATE  ( -- )
      2 CELLS ALLOT   \ set aside 2 slots, user value and read value
    DOES>
      <read_clock>    \ read hardware clock
      OVER CELL+ @    \ get last clock value
      OVER -          \ get the change
      2 PICK +!       \ update user value
      OVER CELL+ !    \ save last read value
;



\ =================================================================
\ PC specific words to set hardware timer to 1193181.667 ticks/second
\ and then later to restore it back to the standard 18.2 ticks/second
\ Note: this won't work in a Windows DOS shell.

HEX

43 CONSTANT TIMER_CONTROL
40 CONSTANT TIMER_0

: initialize_timer ( -- )
    34 TIMER_CONTROL PC!
    0 TIMER_0 PC!
    0 TIMER_0 PC!
;

: restore_timer ( -- )
    36 TIMER_CONTROL PC!
    0 TIMER_0 PC!
    0 TIMER_0 PC!
;

\ =================================================================

DECIMAL

: wait_for_low ( -- )     \ wait until #STATUS bit 7 is low
    BEGIN
      #STATUS pc@ 128 AND 0=
    UNTIL
;

: wait_for_high ( -- )     \ wait until #STATUS bit 7 is high
    BEGIN
      #STATUS pc@ 128 AND
    UNTIL
;

: edge_high? ( -- t/f )     \ return status of #STATUS bit 7
    #STATUS pc@ 128 AND
;


VARIABLE accumulate

\ ===============the period counter================================

: PERIOD ( n -- x )     \ n is number of samples, x is average period

    0 accumulate !
    initialize_timer    \ run the timer at a fast rate

     0 DO
        wait_for_low       \ make sure the level is low first
        <read_clock>
        wait_for_high      \ now poll for a rising edge
        <read_clock> -     \ neglecting rollover
        accumulate +!
      LOOP

   restore_timer

   accumulate @
  
;

\ ===============the frequency counter=============================
\ depending upon your hardware, this simple counter is good to up
\ to about 20 Khz

DOWN-COUNTER count_down

: FREQUENCY ( n -- x )   \ n is number of timer cycles,
                         \ x is average frequency in Hz

    0 accumulate !

    wait_for_low       \ make sure the level is low first
    DUP count_down !

    BEGIN
      edge_high?     \ test to see if edge is high
            IF 1 accumulate +!
               wait_for_low  \ make sure level goes back low
            THEN
      count_down @ 0 <=
    UNTIL

    \ convert counts to Hz
    10 *
    accumulate @
    182 ROT */
;


