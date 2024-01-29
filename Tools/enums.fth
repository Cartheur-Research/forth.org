\ enums.fth          Words to automate the construction of
\                    a list of constants.
    
\                    If the number of enumerations is less than    
\                    the number of bits in a word, then the
\                    enumerations are successive powers of two
\                    otherwise they are successive integers (starting
\                    with 0).  The user can override this and make
\                    the enumerations increment by one by invoking
\                    SEQUENTIAL before ENUMS:
\
\ usage examples:
\    5 enums: one two three four five       \ powers of two constants
\    sequential 3 enums: red green blue     \ successive integers

\  This is a ANS Forth program
\

\               (c) Copyright 1996, Everett F. Carter Jr.
\                   Permission is granted by the author to use
\		    this software for any application provided this
\		    copyright notice is preserved.

\ $Author: skip $
\ $RCSfile: enums.fth,v $
\ $Revision: 1.2 $
\ $Date: 1996/04/18 00:13:27 $

\ ===============================================================

FALSE VALUE enum_powers?
FALSE VALUE inc_by_one

: set_inc ( n -- n )
    1 CELLS 8 * OVER < IF FALSE ELSE inc_by_one INVERT THEN
	               TO enum_powers?
;

: >enum_index ( x -- y )        \ calculate the index value
    enum_powers? IF 1 SWAP LSHIFT THEN
;

\ ===============================================================

: sequential ( -- )       \ if the user invokes this then
   TRUE TO inc_by_one     \ it will override the power of 2 increments
;

: enums: ( n -- )
    set_inc
    0 DO I >enum_index CONSTANT LOOP
    FALSE TO inc_by_one            \ make the user set this each time
;






