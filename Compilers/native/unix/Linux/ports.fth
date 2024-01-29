\ ports.fth          Forth Code to control parallel printer port
\                       see Ken Merk, Forth Dimensions July 1995

\  Uses IOPORTS device plus offset           EFC   March   1996
\  Converted for PFE under Linux             EFC   October 1995

\ This is an ANS Forth program requiring:
\         1. The File Access word set
\         2. The word FLUSH-FILE from the File Access Extensions
\            word set

\  Note: in order to use this code
\        1. Linux should be using the standard polled printer driver
\        2. DB-25 Pin 11 (BUSY) should be tied low, this can be easily
\           achieved by looping pin 11 back to one of pins 18-25
\        3. The proper /dev/parX device is set in INIT-PORT below
\        4. The permissions on that device should be:   crw-rw-rw-

\ ===================================================================


CR .(  PORTS.FTH                   March 1996  )


: PORTS.FTH ;

\ the Linux devices /dev/parX are the parallel ports,
\ the value of X depends upon the address of the port:
\          X       Address
\          0       0x3BC
\          1       0x378
\          2       0x278

: init-port ( -- n )
          S" /dev/ioports" R/W BIN OPEN-FILE
         ABORT" Unable to open I/O ports at /dev/ioports"
;

\ init-port VALUE #IOPORTS

-1 VALUE #IOPORTS

CREATE cbuf    8 ALLOT


: pc!  ( n port -- )
           S>D #IOPORTS REPOSITION-FILE THROW
           cbuf C! cbuf 1 #IOPORTS  WRITE-FILE DROP
	   #IOPORTS FLUSH-FILE DROP
;

: pc@  ( port -- n )
           S>D #IOPORTS REPOSITION-FILE THROW
           cbuf 1 #IOPORTS READ-FILE 2DROP cbuf C@
;

HEX

\ : KILL ( -- )       00 #PORT pc!  ;     \ turn OFF all devices
\ : ALL-ON ( -- )    0FF #PORT pc!  ;     \ turn ON all devices

\ : ON?    ( b -- f ) #PORT pc@ AND 0<> ;  \ get ON status of device
\ : OFF?   ( b -- f ) #PORT pc@ AND 0=  ;  \ get OFF status of device

\ : WRITE ( b -- )    #PORT pc! ;          \ WRITE byte to port
\ : READ  ( -- b )    #PORT pc@ ;          \ READ byte at port

DECIMAL



\ ===================================================================






