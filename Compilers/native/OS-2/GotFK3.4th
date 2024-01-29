\ ---[ Code Addendum 03 ]-----------------------------[12/06/2006]---
\
\          For the Graphics of the First Kind Part III column
\
\                         by Timothy Trussell
\
\ -------------------------------------------------------------------
\ This code is meant for use with the 32Forth system.
\ This is the DOS DPMI version of the compiler in Rick van Norman's
\ OS2FORTH.ZIP package, available on the Taygeta Scientific site.
\ -------------------------------------------------------------------
\ The high level Forth code should work on any other Forth system, if
\ the CODE primitives have been converted to work correctly.
\ -------------------------------------------------------------------
\ Save as CLFCODE3.4TH in your \os2forth\forth directory

\ ---[ Bresenham Line Algorithms ]-----------------------------------
\                        Phil Koopman, Jr.
\                  North Kingstown, Rhode Island
\ -------------------------------------------------------------------

DECIMAL

exists [BRESENHAM] [if]
  forget [BRESENHAM]
[then]

: [BRESENHAM] ;

\ ---[ Code needed from CLFCODE.4TH ]--------------------------------

exists [CLFCODE] not [if]
\ Skips compiling these if the first file is already loaded

code SetMode ( mode -- )
                                \ bx=mode on entry as TOS
                 ax ax  xor     \ ah=0 (function #0)
                 bl al  mov     \ al=
              INT10 #)  call
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

: InitGraph ( mode# -- ) SetMode ;
: CloseGraph ( -- )    3 SetMode ;


code Plot ( x y c -- )
                                \ bx=c on entry as TOS
                 bl al  mov     \ al=pixel color
                    dx  pop     \ dx=y
                    cx  pop     \ cx=x
                 bx bx  xor     \ bx=page#
               12 # ah  mov     \ ah=Plot Pixel function
              INT10 #)  call    \ do it
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

[then]

exists [CLFCODE2] not [if]
\ Skips compiling these if the second file is already loaded

value VSelector $0A000 SEG>DESC to VSelector

code FastPlot ( x y c -- )
                                \ c in bx on entry as TOS
                    bx  push    \ put all parameters onto the stack
                 bp sp  xchg    \ setup the stack frame
                    es  push    \ save registers to be used
                    di  push
        VSelector # ax  mov     \ es=selector
                 ax es  mov
       \ Calculate Y*320+X
       2 cells [bp] di  mov     \ di=x
       1 cells [bp] ax  mov     \ ax=y
                6 # cl  mov
                 ax cl  shl     \ ax=y*64
                 ax di  add     \ di=y*64+x
                2 # cl  mov
                 ax cl  shl     \ ax=y*256
                 ax di  add     \ di=y*320+x
       0 cells [bp] ax  mov     \ al=c
         es: al 0 [di]  mov     \ plot the pixel
                    di  pop     \ restore registers
                    es  pop
                 bp sp  xchg    \ restore stack frame
          3 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

[then]

\ ---[ Variables ]---------------------------------------------------

variable XNow           \ (XNow,YNow) is current cursor location
variable YNow           \ ( 0,0 )     is top/left corner of CRT

variable COLOR          \ Current line draw color
1 COLOR !

variable INCR1
variable INCR2
variable DX
variable DY

\ ---[ Move-Cursor ]-------------------------------------------------
\ Move cursor location before a draw

: Move-Cursor  ( X Y -> )
  YNow !
  XNow !
;

\ ---[ Point ]-------------------------------------------------------
\ Point Plot using color variable

: POINT   ( X Y -> )
  COLOR @               \ x y c
  FastPlot              \ --
;

\ ---[ B-Point ]-----------------------------------------------------
\ For Bresenham line drawing use

: B-POINT ( X Y DELTA -> )
  >R  2DUP  POINT R> ;

\ ---[ Primitives ]--------------------------------------------------
\ Bresenham line draw primitives

: +X   ( X1 Y1 DELTA -> X2 Y2 DELTA ) ROT 1+  -ROT ;
: -X   ( X1 Y1 DELTA -> X2 Y2 DELTA ) ROT 1-  -ROT ;
: +Y   ( X1 Y1 DELTA -> X2 Y2 DELTA ) SWAP 1+ SWAP ;
: -Y   ( X1 Y1 DELTA -> X2 Y2 DELTA ) SWAP 1- SWAP ;

\ ---[ LINE for SLOPE=0 -- HORIZONTAL ]------------------------------
\ Assumes DX and DY are already setup

: LINE0   ( NEWX NEWY -> )
   ( PICK MIN X )
   OVER XNow @ > IF     ( CURRENT CURSOR AT MIN X  )
     2DROP
     XNow @
     YNow @
   THEN
   2DUP POINT 0         ( DUMMY DELTA VALUE )
   DX @ 0 DO
     +X B-POINT
   LOOP
   DROP
   2DROP
;

\ ---[ LINE for SLOPE=INFINITY -- VERTICAL ]-------------------------
\ Assumes DX and DY are already setup

: LINEZ   ( NEWX NEWY -> )
   ( PICK MIN Y )
   DUP YNow @ > IF      ( CURRENT CURSOR AT MIN Y  )
     2DROP
     XNow @
     YNow @
   THEN
   2DUP POINT 0         ( DUMMY DELTA VALUE )
   DY @ 0 DO
     +Y B-POINT
   LOOP
   DROP
   2DROP
;

\ ---[ LINE for -INFINITY<SLOPE<-1 ]---------------------------------
\ Assumes DX and DY are already setup

: LINE-Z<M<-1  (  NEWX NEWY -> )
   DX @ 2* INCR1 !
   DX @ DY @ - 2* INCR2 !
   ( PICK MIN Y )
   DUP YNow @ > IF      ( CURRENT CURSOR AT MIN Y )
     2DROP
     XNow @
     YNow @
   THEN
   2DUP POINT
   ( COMPUTE D )
   INCR1 @  DY @ -                      \ STACK: ( X Y DELTA --- )
   DY @ 0 DO
     DUP 0< IF          ( D < 0 )
       +Y B-POINT INCR1 @ +
     ELSE               ( D >= 0 )
       -X +Y B-POINT INCR2 @ +
     THEN
   LOOP
   DROP
   2DROP
;

\ ---[ LINE for 0<SLOPE<1 ]------------------------------------------
\ Assumes DX and DY are already setup

: LINE0<M<1    ( NEWX NEWY -> )
   DY @ 2* INCR1 !
   DY @ DX @ - 2* INCR2 !
   ( PICK UP MIN X )
   OVER XNow @ >
   IF  ( CURRENT CURSOR AT MIN X )
     2DROP
     XNow @
     YNow @
   THEN
   2DUP POINT
   ( COMPUTE D )
   INCR1 @ DX @ -               \ STACK:  ( X Y DELTA --- )
   DX @ 0 DO
     DUP 0< IF          ( D < 0 )
       +X B-POINT INCR1 @ +
     ELSE               ( D >= 0 )
       +X +Y B-POINT INCR2 @ +
     THEN
   LOOP
   DROP
   2DROP
;

\ ---[ LINE for 1<=SLOPE<INFINITY ]----------------------------------
\ Assumes DX and DY are already setup

: LINE1<M<Z   ( NEWX NEWY -> )
   DX @ 2* INCR1 !
   DX @ DY @ - 2* INCR2 !
   ( PICK MIN Y )
   DUP YNow @ > IF      ( CURRENT CURSOR AT MIN Y )
     2DROP
     XNow @
     YNow @
   THEN
   2DUP POINT
   ( COMPUTE D )
   INCR1 @ DY @ -                       \ STACK:  ( X Y DELTA --- )
   DY @ 0 DO
     DUP 0< IF          ( D < 0 )
       +Y B-POINT INCR1 @ +
     ELSE               ( D >= 0 )
       +X +Y B-POINT INCR2 @ +
     THEN
   LOOP
   DROP
   2DROP
;

\ ---[ LINE for -1<SLOPE<0 ]-----------------------------------------
\ Assumes DX and DY are already setup

: LINE-1<M<0   (  NEWX NEWY -> )
   DY @ 2* INCR1 !
   DY @ DX @ - 2* INCR2 !
   ( PICK MIN X )
   OVER XNow @ > IF     ( CURRENT CURSOR AT MIN Y )
     2DROP
     XNow @
     YNow @
   THEN
   2DUP POINT
   ( COMPUTE D )
   INCR1 @ DX @ -                       \ STACK: ( X Y DELTA --- )
   DX @ 0 DO
     DUP 0< IF          ( D < 0 )
       +X B-POINT INCR1 @ +
     ELSE               ( D >= 0 )
       +X -Y B-POINT INCR2 @ +
     THEN
   LOOP
   DROP
   2DROP
;

\ ---[ Bresenham Line ]----------------------------------------------
\ Main Bresenham Prologue and calling word

: (LINE)  ( XNEW YNEW -> )
  2DUP                  ( EXTRA COPY USED FOR FINAL Move-Cursor )
  OVER XNow @ - DUP ABS DX !
  OVER YNow @ - DUP ABS DY !
  XOR 0<                ( DETERMINE IF SIGNS ARE DIFFERENT )
  DY @ IF
    DX @ IF             ( NOT HORIZONTAL OR VERTICAL )
      IF                ( NEGATIVE SLOPE )
        DX @ DY @ > IF
          LINE-1<M<0
        ELSE
          LINE-Z<M<-1
        THEN
      ELSE  ( POSITIVE SLOPE )
        DX @ DY @ > IF
          LINE0<M<1
        ELSE
          LINE1<M<Z
        THEN
      THEN
    ELSE ( VERTICAL )
      DROP
      LINEZ
    THEN
  ELSE ( HORIZONTAL )
    DROP
    LINE0
  THEN
  Move-Cursor
;

\ My modification to this wordset - Tim

: Line ( x1 y1 x2 y2 c -- )
  Color !               \ x1 y1 x2 y2
  >R >R                 \ x1 y1
  Move-Cursor           \ --
  R> R>                 \ x2 y2
  (Line)                \ --
;

\ ---[ End of CLFCODE3.4TH ]-----------------------------------------

