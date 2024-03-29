
>From oster@ucblapis.berkeley.edu (David Phillip Oster) Wed Jun 18 12:47:24 1986
Relay-Version: version B 2.10.2 9/18/84; site bdmrrr.UUCP
Path: bdmrrr!rlgvax!seismo!lll-crg!styx!nike!ucbcad!ucbvax!jade!ucblapis!oster
From: oster@ucblapis.berkeley.edu (David Phillip Oster)
Newsgroups: net.lang.forth
Subject: Oster's Forth Structure Package
Message-ID: <863@jade.BERKELEY.EDU>
Date: 18 Jun 86 16:47:24 GMT
Date-Received: 20 Jun 86 01:13:57 GMT
Sender: usenet@jade.BERKELEY.EDU
Reply-To: oster@ucblapis.UUCP (David Phillip Oster)
Distribution: net
Organization: University of California, Berkeley
Lines: 183
Keywords: STRUCT RECORD DEFSTRUCT

Here's what you've been asking for, here is the source code to define
structures in Forth. I first wrote this code almost two years ago, when I
was in the process of writing my own implementation of Forth-83 for the
Macintosh.  Menu Clock is written in that Forth.
( Pascal Record Package,I ( David Phillip Oster 3/4/85 )
\ Structures in Forth are almost as simple as arrays in Forth.
\ Here it is. (Note that the actual code, without the comments, is only 16
\ lines long.)

\ This structure package is 
\ Copyright (c) 1985, 1986 David Phillip Oster. All Rights Reserved
\ You may use it as you see fit except:
\ 1 ) all copies of the source code of the package must contain 
\ this copyright message.
\ 2 ) If you describe this idea you must give David Oster
\ full credit for its invention.


\ ****************** General Comments *********************
\ Structure:
\   This file consists of:
\   General Coments: this overview
\   Implementation: the underlying implementation of the record package
\   Example 1: Declaring some macintosh types.
\   Example 2: Using those types to do some mac programming
\ 
\ Use:
\ This package defines two defining words, 
\ "RECORD"	( starts a pascal-like record-type definition)
\ "ENDREC:"	( ends, and gives a name to the record-type)
\ 
\ and also
\ 
\ "SIZEOF("	( takes a type name and returns the size fo that type.)
\ 
\ and lastly:
\ 
\ "MAKEREC:"	( record types are defined in terms of other 
\ 			record types.
\ 				To start the system off, you need a more primitive way of 
\ 			creating record types. )
\ 
\ Features:
\ 	Record-types can be used inside other record-types to declare
\ field names.  Record-types can also be used to declare variables in the
\ dictionary of that type.
\ 
\ 	The code generated by the package is optimized: field names that
\ merely add 0 to the record base address generate no run-time code!
\ 
\ Record-types, created with this package can be used in the definition of
\ yet other records. Record-types can also be used to declare variables
\ of that type.
\ Field names are also defined by this package.  Field names can be applied
\ to record variables to give the address of the beginning of that field.
\ 
\ Naming Conventions:
\   field names start with a period.  Record-Types, like all defining words,
\ end with a colon. I pretty print against the right-edge, since the verbs
\ in forth are on the right.
\ 
\ Limitations:
\ The Forth-83 vocabulary structure is NOT used, so you can apply field 
\ names for type A to a record of type B, and no-one but you will ever 
\ catch the mistake (this is no worse than most of Forth.) 
\ The underlying implementation of the record package should really be in
\ a hidden vocabulary.
\ Field names are just words in the vocabulary, so they should be unique.
\ Some-one should extend this so that all the field names of a record
\ are grouped in that record's "record-type vocabulary"
\ using a record variable sets the "record-type vocabulary"
\ this would allow a record type A to have a field named ".foo" and a 
\ second record-type B to have a field name ".foo" without B's definition
\ overriding A's.  (This package overrides.)
\
\ ****************** The Structure package ******************
VARIABLE INRECORD  ( Holds a flag - are we defining a record or a variable?)

( WALIGN - if you need it align to word boundary )
\ In Mac Pascal, successive byte sized fields are packed, but you
\ skip a byte if you have an odd number of byte sized fields and the next 
\ field is larger than a byte.  
: WALIGN ( AmtUsed MySiz - AdjustUse MySiz )  DUP 1 = NOT IF
              SWAP 1+ [ HEX ] FFFE [ DECIMAL ] AND SWAP THEN ;

( DOOFFSETR - Runtime of a .Field name )
: DOOFFSETR DOES> ( adrRecord adrOffset - adrRecord+Offset )
      @  + ;

( DOPASREC - run time actions for PascalType: )
\ two major cases: declaring a .Field, or declaring a var
\ If first field, make .PascalField a compileTime noop
: DOPASREC  ( -- ) DOES> @ CREATE  INRECORD @ IF
                           WALIGN OVER ?DUP IF
                              , DOOFFSETR ELSE
          -2 DP +! ['] NOOP @ , IMMEDIATE THEN
                                         +  ELSE
                                      ALLOT THEN ;

\ ****************** Stucture package top level ******************
\ begin a record definition
: RECORD ( - 0 | intialize vars ) INRECORD ON  0 ;

\ takes number of bytes, which is the size of the field
: MAKEREC: ( n - ; name )  CREATE , DOPASREC ;

\ interested in fields, not in the final record, so just end
: ENDREC   ( n - |Used when just defining fields)
                                  INRECORD OFF DROP ;
\ create the record
: ENDREC:  ( n - ; name )         INRECORD OFF MAKEREC: ;

\ takes a type-name from the input
: SIZEOF(  ( - n ;typename) 
	' >BODY @ STATE @ IF LITERAL THEN ; IMMEDIATE

\ ****************** An example, some Macintosh structures ******************
( Mac FontInfoRec Point record definitions    ( 12/26/84 dpo)
4 	MAKEREC: LINT:		\ LongInt
2 	MAKEREC: INT:		\ Integer  
1 	MAKEREC: BOOL:		\ Boolean
8 	MAKEREC: Pattern:   
32	MAKEREC: Bits16:
4 	MAKEREC: RgnHandle:
4 	MAKEREC: Handle:      
4 	MAKEREC: PTR:		\ Ptr

  RECORD
    INT: .ASCENT
    INT: .DESCENT
    INT: .WIDMAX
    INT: .LEADING
  ENDREC: FontInfoRec:

  RECORD
    INT: .V
    INT: .H
  ENDREC: Point:

  RECORD
    Point: .topLeft
    Point: .botRight
  ENDREC: Rect:

\ Other names for fields of rect:
: .bottom  .botright   .v ;
: .top     .topLeft    .v ;
: .right   .botright   .h ;
: .left    .topleft    .h ;

  RECORD
    PTR:   .BASEADDR
    INT:   .ROWBYTES
    RECT:  .BOUNDS
  ENDREC: BitMap:

\ ****************** Example, continued ******************
Point: SavePen
SavePen GetPen
 100 100 MoveTo
 SavePen .x @ SavePen .y @ LineTo

Rect: 4Square

: CopyPoint ( &Point &Point - ) SIZEOF( Point: CMOVE ;

SavePen		4Square .topLeft	CopyPoint
SavePen		4Square .botRight	CopyPoint

4Square .bottom 4 +!
4Square .right  4 +!

4Square FrameRect

--- David Phillip Oster		-- "The goal of Computer Science is to
Arpa: oster@lapis.berkeley.edu  -- build something that will last at
Uucp: ucbvax!ucblapis!oster     -- least until we've finished building it."
U.S.Mail:
David Oster
Mosaic Codes
Suite 1036
2000 Center St.
Berkeley, Ca. 94704


