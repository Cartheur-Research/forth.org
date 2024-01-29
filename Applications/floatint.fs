0 [if] ==============================================================
     File Name: FloatInt.fs
        Author: Timothy Trussell
          Date: 05/12/2010
   Description: New data type holding a Float and Integer value
              : A dual variable construct
  Forth System: gforth-0.7.0
=====================================================================

---------------------------------------------------------------------
                              FloatInt
                              IntFloat
---------------------------------------------------------------------

---[ Concept Description ]-------------------------------------------

Create a new data type that contains both a 64-bit (8 byte) Float and
a 32-bit Integer (4 byte) - a dual variable.

Create store/fetch words that access both the Floating Point and the
Integer values of the number being worked with.

---[ Implementation ]------------------------------------------------

There would be two versions:

        1. If you are storing the Integer value it creates the
           Floating Point value and stores it also

        2. If you are storing the Floating Point value it creates the
           Integer value and stores it also

Names:

    Short Form:

        IF! IF@         for accessing the Integer portion
        FI! FI@         for accessing the Floating Point portion

    Long Form:

        IntFloat! IntFloat@
        FloatInt! FloatInt@

---[ Calling conventions ]-------------------------------------------

    floatint:

        <rvalue> floatint "variable name"

        where <rvalue> is a floating point value and "variable name"
        is the name to be created in the dictionary for this dual
        variable.

    intfloat:

        <value> intfloat "variable name"

        where <value> is an integer value and "variable name" is the
        name to be created in the dictionary for this dual variable.

    if!:

        <n> <addr> if!

        where <n> is an integer value and <addr> is the address of
        the dual variable to store the data into.

    fi!:

        <r> <addr> fi!

        where <r> is a floating point value and <addr> is the address
        of the dual variable to store the data into.

    if@:

        <addr> if@

        returns the integer value of the dual variable at <addr>.

    fi@:

        <addr> fi@

        returns the floating point value of the dual variable at
        <addr>.

---[ Example of usage ]----------------------------------------------

        PI 2e0 F* floatint PI*2
        15 intfloat VGA-White

        PI*2 fi@ f.           \ loads/prints the floating point value
        PI*2 if@ .                   \ loads/prints the integer value

        VGA-White if@ .              \ loads/prints the integer value
        VGA-White fi@ f.      \ loads/prints the floating point value

---[ Coding ]------------------------------------------------- [then]

    struct
      float% field .float
      cell%  field .integer
    end-struct floatint%

    : floatint ( rvalue -- )
      create
      fdup                           \ copy for conversion to integer
      floatint% %allot >R      \ allocate the struct memory, save ptr
      R@ .float F!                    \ save the floating point value
      F>S                                        \ convert to integer
      R> .integer !                          \ save the integer value
    ;

    : intfloat ( ivalue -- )
      create
      dup                     \ copy for conversion to floating point
      floatint% %allot >R      \ allocate the struct memory, save ptr
      R@ .integer !                          \ save the integer value
      S>F                                 \ convert to floating point
      R> .float F!                    \ save the floating point value
    ;

    : if! ( n addr -- )                \ store - integer value passed
      >R                                      \ save the dest pointer
      dup                                            \ copy the value
      R@ .integer !                          \ save the integer value
      S>F                                 \ convert to floating point
      R> .float F!                    \ save the floating point value
    ;

    : fi! ( r addr -- )
      >R                                      \ save the dest pointer
      fdup                                           \ copy the value
      R@ .float F!                    \ save the floating point value
      F>S                                        \ convert to integer
      R> .integer !                          \ save the integer value
    ;

    : if@ ( addr -- n ) .integer @ ;              \ fetch the integer
    : fi@ ( addr -- r ) .float F@ ;                 \ fetch the float

    : intfloat@ if@ ;
    : intfloat! if! ;
    : floatint@ fi@ ;
    : floatint! fi! ;

\ --------------------------------------------------[End FloatInt]---

