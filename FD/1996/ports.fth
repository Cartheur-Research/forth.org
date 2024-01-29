\ ports.fth          Forth Code to control parallel printer port
\                       see Ken Merk, Forth Dimensions July 1995

\  Uses IOPORTS device plus offset           EFC   March   1996
\  Converted for PFE under Linux             EFC   October 1995

\ This is an ANS Forth program requiring:
\         1. The File Access word set
\         2. The word FLUSH-FILE from the File Access Extensions
\            word set

\  Note: in order to use this code
\        1. The device /dev/ioports should exist, it is a copy
\           of standard device /dev/port
\        2. The permissions on the /dev/ioports device should be:
\           crw-rw-rw-, and the group should be 'users' or
\           a locally defined group

\ $Author:   skip  $
\ $Workfile:   ports.fth  $
\ $Revision:   1.0  $
\ $Date:   11 Jul 1996 10:40:44  $

\ ===================================================================



: init-port ( -- n )
          S" /dev/ioports" R/W BIN OPEN-FILE
         ABORT" Unable to open I/O ports at /dev/ioports"
;


\ init-port VALUE #IOPORTS

-1 VALUE #IOPORTS

CREATE cbuf    8 ALLOT

: close-port ( -- )
         #IOPORTS CLOSE-FILE
         ABORT" Unable to close I/O ports at /dev/ioports"
;

: pc!  ( n port -- )
           S>D #IOPORTS REPOSITION-FILE THROW
           cbuf C! cbuf 1 #IOPORTS  WRITE-FILE DROP
	   #IOPORTS FLUSH-FILE DROP
;

: pc@  ( port -- n )
           S>D #IOPORTS REPOSITION-FILE THROW
           cbuf 1 #IOPORTS READ-FILE 2DROP cbuf C@
;




\ ===================================================================






