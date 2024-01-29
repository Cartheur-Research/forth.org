\ spi.fth          SPI using the PC Parallel Port

\ This is an ANS Forth program for SPI I/O on the PC
\ parallel port under MSDOS or Linux requiring:
\         1. The File Access word set
\         2. the conditional compilation words in the
\             PROGRAMMING-TOOLS word set
\         3. The word USEC ( us -- )  is required to cause a delay
\            for the specified number of microseconds
\         4. For use under MSDOS the word
\             : MSDOS  ;
\            must be defined before loading this file
\         5. It is assumed that the inverse of COUNT is DROP 1-

\ Uses the following I/O lines of #DATA (base)
\ and #STATUS (base + 1)
\
\     Master                    Slave
\  Pin      Bit     Name      Pin    Bit
\   10    Status-6  MISO       2     Data-0
\    3     Data-1   Clock     12    Status-5
\    2     Data-0   MOSI      10    Status-6
\    4     Data-2   Select    13    Status-4  ( active low )
\   25              Ground    25

\ For PC-PC communications the Select is not really necessary,
\ but for the Motorola QSM and other devices its needed

\               (c) Copyright 1996, Everett F. Carter Jr.
\                   Permission is granted by the author to use
\                   this software for any application provided this
\                   copyright notice is preserved.

\ $Author:   skip  $
\ $Workfile:   spi.fth  $
\ $Revision:   1.1  $
\ $Date:   13 Jul 1996 02:36:36  $

\ ===================================================================


\ adapted from the Forth Scientific Library
\ assumes that the inverse of COUNT is DROP 1-

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


HEX

\ bitmasks and shift offsets

40 CONSTANT READ_MASK
 6 CONSTANT READ_SHIFT

 1 CONSTANT WRITE_MASK
 0 CONSTANT WRITE_SHIFT

10 CONSTANT SELECT_MASK

2          VALUE clock_mask
0          VALUE transfer-byte    \ execution vector
0          VALUE select           \ execution vector
0          VALUE deselect         \ execution vector

DECIMAL


16 CONSTANT BUFSIZE       \ the size of the I/O buffers

    BUFSIZE VALUE N

CREATE inbuf  BUFSIZE ALLOT
CREATE outbuf BUFSIZE ALLOT

: show-buffer ( addr n -- )

  CR
  0 DO I OVER + C@ .  LOOP
  DROP
  CR
;

\ generate additive sequence, for building dummy data
: gas  ( addr n -- )  0 DO I OVER  C! 1+ LOOP  DROP ;


: spi-setup ( -- )

    initialize           \ open/setup I/O port

    4 #DATA pc!          \ set idle levels, and select off
;


: strobe-clock ( -- )         \ drive clock line high then low
                              \ master does this

    #DATA pc@              \ get current value

    \ generate above value with clock high and clock low
    clock_mask OR DUP clock_mask XOR
    SWAP #DATA pc!

    ( pause momentarily here )
    1 usec

    #DATA pc!

    1 usec

;

: wait-on-clock-hi ( -- )      \ wait for leading clock edge
                               \ slave does this

    \ now wait for clock to go high
    BEGIN
	#STATUS pc@ clock_mask AND
    UNTIL

;

: wait-on-clock-lo ( -- )      \ wait for trailing clock edge
                               \ slave does this

    \ wait here for the clock to be low
    BEGIN
	#STATUS pc@ clock_mask AND 0=
    UNTIL


;

: read-bit ( -- x )         \ lsb of x is new bit

    #STATUS pc@ READ_MASK AND
    READ_SHIFT RSHIFT

;

: write-bit ( x -- )         \ write lsb of x
                             \ handles inverting of bit by hardware
    
    WRITE_SHIFT LSHIFT
    WRITE_MASK AND           \ shift and mask to get just bit to send

    WRITE_MASK -1 XOR
    #DATA pc@ AND             \ set output bit to 0

    OR
    #DATA pc!                \ send output bit
;

: master-transfer-byte ( x -- y )     \ write x, read y

    0

    0 7 DO
           1 LSHIFT
	   OVER I RSHIFT write-bit   \ write before rising edge
	   strobe-clock
	   read-bit OR               \ read after falling edge
    -1 +LOOP

    SWAP DROP
;

: slave-transfer-byte ( x -- y )       \ write x, read y

    0

    0 7 DO
          1 LSHIFT
	  wait-on-clock-hi       \ read after rising edge
          read-bit OR
	  OVER I RSHIFT
	  wait-on-clock-lo       \ write after falling edge
	  write-bit
        
	-1 +LOOP

    SWAP DROP

;

: assert-select ( -- )   \ master
    0 #DATA pc!          \ set idle levels, and select ON
;

: deassert-select ( -- )
    4 #DATA pc!          \ set idle levels, and select off
;

: wait-on-select ( -- )  \ slave

    wait-on-clock-lo

    BEGIN
	#STATUS pc@ SELECT_MASK AND 0=
    UNTIL
;

: nothing ;




: transfer-data ( n -- )     \ send outbuf data, receive to inbuf

    select EXECUTE     \ select once for whole loop,
                       \ some devices prefer to select for
		       \ each individual transfer, move this
		       \ inside the loop in that case

    0 DO

	outbuf I + C@
	transfer-byte EXECUTE
	inbuf I + C!
    
    LOOP

    deselect EXECUTE    \ see note on select above, move this
                        \ inside the loop too if select is moved

;


: setup ( -- )

    inbuf BUFSIZE 0 FILL
    outbuf  BUFSIZE gas

;

\ ===================================================================

: test ( -- )

    setup


    BUFSIZE transfer-data


    close-port

    inbuf BUFSIZE show-buffer
;



: master ( -- )

    2 TO clock_mask

    ['] master-transfer-byte TO transfer-byte
    ['] assert-select        TO select
    ['] deassert-select      TO deselect

    spi-setup

;


: slave ( -- )


    32 TO clock_mask

    ['] slave-transfer-byte TO transfer-byte
    ['] nothing             TO deselect

    \ use 'nothing' below if NOT using
    \ the select line for the slave (e.g. PC to PC )
    ['] wait-on-select      TO select
    \ ['] nothing             TO select


    spi-setup

;


\ usage:   master test
\ or:      slave  test





