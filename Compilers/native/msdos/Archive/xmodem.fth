
XMODEM PROTOCOL ON FORTH-83
Wilson M. Federici
1208 NW Grant
Corvallis OR 97330  (503) 753-6744
Version 1.01  9/85

A "generic" set of words to implement the XMODEM protocol
on Forth-83 systems.  User must supply i/o functions and
"file" ( or block-sequence ) handlers as described.

Timing constants TXTIME , WXTIME , STIME , LTIME are very
machine-dependent.  The values do not need great accuracy,
+/- 25% should work.

The counts for MAXTIME and RETRIES and the timeout
counts passed to WAITCAN at various places determine how
"patient" your computer will be toward the remote machine;
the values given seem about right for "spasmodic" systems
such as Compuserve and could be reduced for other uses.

The top-level words RECEIVE and TRANSMIT assume a "file" has
been opened for WRITE128 or READ128 , respectively, and that
the "file" will be closed after they finish.  Both functions
will eventually abort if the remote system does not respond,
but there is no limit to the number of retries after a 
faulty transmission of a record.  The keyboard is checked
frequently for a user-abort via control-X.

External functions:
Keyboard input
    ?KEY   (   --  char )
    Return char from user's keyboard, if any waiting, else
    return zero.  The 9 high-order bits of char must be zero.
Transmitter
    ?XMT   (   --  flag )
    Return true if transmitter is ready to accept a byte.
    XMT    ( n --  )
    Transmit 8 ( not 7 ! ) low-order bits of n to
    remote.  Called only after ?XMT returns true.
Receiver
    ?RCV   (   --  flag )
    Return true if receiver has a byte waiting.
    RCV    (   --  n )
    Return a byte from the remote.  The 8 high-order bits
    should be zero.  Called only after ?RCV returns true.
"Files"
    READ128 ( buf-addr -- flag )
    Read up to 128 bytes from "file" into buffer at 
    buf-addr, return flag non-zero if at least one byte
    was placed in buffer, or zero if all done.  Note 
    that buffer is zero-filled before READ128 is called.
    WRITE128 ( buf-addr --  )
    Write 128 bytes from buffer at buf-addr to "file".
    Note that buffer contents are padded to a full
    128 bytes with nulls if necessary, but since the
    protocol allows binary data the trailing nulls
    may be significant.
Errors
    ERROR-EXIT  (  --  )
    Called after timeout, cancellation, or keyboard abort,
    should close "file" and return control to user.


FORTH DEFINITIONS DECIMAL

24 CONSTANT CAN    1 CONSTANT SOH    4 CONSTANT EOT
21 CONSTANT NAK    6 CONSTANT ACK

VARIABLE REC# ( current record number )
: REC#+1  REC# @ 1+ 255 AND REC# ! ;

VARIABLE CHKSUM
CREATE REC-BUF 128 ALLOT
: RUN-SUM  ( calculate CHKSUM ) 0  REC-BUF 128 OVER + SWAP
   DO I C@ + LOOP   255 AND CHKSUM ! ;

: ?ABORT  ( from keyboard ) ?KEY  CAN = IF
     CR ." USER-ABORT" ERROR-EXIT THEN  ;

: ?CANCEL  ( char -- char      abort if CAN )
  DUP CAN = IF CR  ." Cancelled" ERROR-EXIT THEN ;

7000 CONSTANT TXTIME  ( non-critical, say ~1 sec. )

: TX  ( char --        transmit, error on timeout )
  TXTIME BEGIN  ?XMT 0= WHILE
     1- DUP 0= IF CR ." XMT TIMEOUT" ERROR-EXIT THEN
  REPEAT  DROP XMT  ;

6000 CONSTANT WXTIME ( gives WAIT ~1 sec. per unit delay )

: WAIT  ( seconds --  char   error on timeout )
  WXTIME  BEGIN  ?RCV  0= WHILE
     1- ?DUP 0= IF  ( one delay-loop done )
           1- DUP 0= IF ( multiples done )
                    CR ." RCV TIMEOUT" ERROR-EXIT THEN
     ( else go again )  ?ABORT WXTIME THEN
  REPEAT  2DROP RCV  ;

: WAITCAN  ( seconds -- chr   do  WAIT  and check for CAN )
  WAIT ?CANCEL  ?ABORT ;

700 CONSTANT STIME  ( gives SWAIT ~0.1 sec. )
32000 CONSTANT LTIME ( gives SWAIT ~5 sec. )
VARIABLE TIMELIMIT ( either ShortTIME or LongTIME )

( a simple "adaptive" timeout to limit lockup )
: SWAIT (  -- char/-1    -1 means timeout )
  TIMELIMIT @ BEGIN  1- ?DUP WHILE
         ?RCV IF DROP RCV EXIT THEN
  REPEAT ?ABORT -1  STIME TIMELIMIT ! ;

: LWAIT  LTIME TIMELIMIT ! SWAIT ;

: CLEAN-LINE ( gobble input )
  STIME TIMELIMIT ! BEGIN SWAIT -1 = UNTIL ;

( Device control is not strict XMODEM but is used. )
17 CONSTANT DC1    19 CONSTANT DC3

: ?DEVICE-CONTROL  ( char1 -- char2     handle DC3 )
  DUP DC3 = IF  DROP CR ." Paused-" BEGIN  LWAIT DC1 - WHILE
  ?ABORT REPEAT 15 WAITCAN   THEN ;

: FILL-BUF  (  -- flag   zero if nothing in buffer )
  REC-BUF  128 ERASE
  REC-BUF  READ128   ( -- flag ) ;
: TXREC  ( --     transmit current rec. )
  RUN-SUM
  BEGIN
    CR ." Transmitting rec# " REC# ?
    CLEAN-LINE
    SOH  TX   REC# @ DUP  TX  NOT TX
    REC-BUF 128 OVER + SWAP DO I C@ TX  LOOP
    CHKSUM @ TX  15 WAITCAN   ?DEVICE-CONTROL  ACK -
  WHILE   CR ." Transmission error"   REPEAT ;

: TRANSMIT  (   --    send the opened file )
  CLEAN-LINE
  1 REC# !
  CR ." Synchronizing-"
  BEGIN 30 WAITCAN  NAK = UNTIL
  BEGIN  FILL-BUF WHILE  TXREC REC#+1   REPEAT
  CR ." Transmitting EOT"
  BEGIN  EOT TX  10 WAITCAN ACK = UNTIL
  CR ." EOT acknowledged" CR  ;

: RXREC  (  -- rec#, ACK/NAK  )
  SWAIT  SWAIT   ( rec# and comp. )
  REC-BUF 128 OVER + SWAP DO
       SWAIT I C! LOOP
  SWAIT  RUN-SUM  CHKSUM @ -
     IF CR ." Checksum error" DROP NAK
     ELSE  OVER NOT XOR 255 AND
          IF CR ." Complement error" NAK
          ELSE ACK
          THEN
     THEN  ;

VARIABLE RESPONSE  ( ACK/NAK for receive routine )

2 CONSTANT MAXTIME  ( per retry, in LTIME units )

3 CONSTANT RETRIES  ( of MAXTIME )

: WAITREC  (  -- n, ACK/NAK/EOT )  CLEAN-LINE RESPONSE @ TX
  RETRIES MAXTIME BEGIN  LWAIT DUP SOH - WHILE
    ?ABORT  ?CANCEL  DUP EOT = IF SWAP DROP EXIT THEN
    -1 = IF ( timed out ) 1- ?DUP ( maxtime )
           0= IF ( maxtime done )  1-  DUP  ( retries )
                0= IF CR ." Can't sync"  ERROR-EXIT
                   THEN ( else ) MAXTIME CLEAN-LINE NAK TX
              THEN
         THEN
  REPEAT  2DROP DROP RXREC  ;

: SEQ-CHECK  ( rec# -- )  DUP REC# @
  = IF REC-BUF WRITE128 REC#+1
  ELSE REC# @ - 1+ IF CR ." Sequence error"  ERROR-EXIT THEN
            ( else just a duplicate )
  THEN ;

: RECEIVE  ( into open file )
  1 REC# !  NAK RESPONSE !
  CLEAN-LINE
  BEGIN  CR ." Waiting for rec# " REC# ?
     WAITREC  DUP RESPONSE !  DUP EOT - WHILE
     ACK = IF SEQ-CHECK ELSE DROP THEN ?ABORT
  REPEAT 2DROP  ACK TX  CR ." EOT received"  ;

( end of code )


