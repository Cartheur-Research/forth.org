\ ---[ Code Addendum 04 ]-----------------------------[12/07/2006]---
\
\          For the Graphics of the First Kind Part IV column
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
\ Save as CLFCODE4.4TH in your \os2forth\forth directory

exists [CLFCODE4] [if]
  forget [CLFCODE4]
[then]

: [CLFCODE4] ;

\ Load previous modules

exists [CLFCODE3] not [if]
\ Skip compiling if already loaded
  include clfcode3.4th
[then]

\ ---[ Allocate Arrays ]---------------------------------------------

create Pal1[] here 256 3 * dup allot 0 fill   \ the work palette
create Pal2[] here 256 3 * dup allot 0 fill   \ the original palette
create Pal3[] here 256 3 * dup allot 0 fill   \ a temp work palette

\ This declares the Pal1 array. 0 to 255 signify the colors of the
\ palette, 1 to 3 signifies the Red, Green and Blue values. I am going
\ to use this as a sort of "virtual palette", and alter it as much as
\ I want, then suddenly bang it to screen. Pal2 is used to "remember"
\ the original palette so that we can restore it at the end of the
\ program.

\ A trio of structure-simulation words for accessing the palette data

: .red   ( &pal -- &pal.r ) 0 + ;
: .green ( &pal -- &pal.g ) 1 + ;
: .blue  ( &pal -- &pal.b ) 2 + ;

\ ---[ CPortOut/CPortIn ]--------------------------------------------
\ This code utilizes I/O Port access, so we will need to add the
\ following words to the Forth dictionary before the code here can be
\ used:

\ ---[Note]----------------------------------------------------------
\ Because of a flaw in the 32Forth assembler, I have to specifically
\ insert the $EE code in place of assembling the AL DX OUT sequence
\ in the CPortOut word, as the 32Forth assembler mistakenly assembles
\ $EF (AX DX OUT) instead of $EE.
\ ------------------------------------------------------[End Note]---

code CPortOut ( c addr -- )
                                \ bx=addr on entry as TOS
                 bx dx mov      \ dx=addr
                    ax pop      \ al=c
                       $EE C,   \ OUT DX,AL
                    bx pop      \ get new TOS
                    end-code
                    NO-EXPAND

code CPortIn ( addr -- c )
                                \ bx=addr on entry as TOS
                 bx dx mov      \ dx=addr
                 ax ax xor      \ zero eax just to be safe
                 dx al in       \ input the byte
                 ax bx mov      \ leave c as TOS in bx
                    end-code
                    NO-EXPAND

exists RND not [if]

\ ---[ Random Number Generator ]-------------------------------------
\ D. H. Lehmers Parametric multiplicative linear congruential random
\ number generator is implemented as outlined in the October 1988
\ Communications of the ACM ( V 31 N 10 page 1192)

     16807 CONSTANT A
2147483647 CONSTANT M
    127773 CONSTANT Q   \ m a /
      2836 CONSTANT R   \ m a mod

create %SEED 123475689 ,

\ Returns a full cycle random number
: RAND ( -- rand )  \ 0 <= rand < 4,294,967,295
   %SEED @ Q /MOD ( lo high)
   R * SWAP A * 2DUP > IF  - ELSE  - M +  THEN  DUP %SEED ! ;

\ Returns single random number less than n
: RND ( n -- rnd )  \ 0 <= rnd < n
   RAND SWAP MOD ;

[then]

\ ---[ WaitRetrace ]-------------------------------------------------
\ This waits until you are in a Verticle Retrace ... this means that
\ all screen manipulation you do only appears on screen in the next
\ verticle retrace ... this removes most of the "fuzz" that you see
\ on the screen when changing the palette. It unfortunately slows
\ down your program by "synching" your program with your monitor
\ card ... it does mean that the program will run at almost the same
\ speed on different speeds of computers which have similar monitors.
\ In our SilkyDemo, we used a WaitRetrace, and it therefore runs at
\ the same (fairly fast) speed when Turbo is on or off.

code WaitRetrace ( -- )
            $03DA # dx  mov
1 L:
                 dx al  in
                8 # al  test
                  1 L#  jnz
2 L:
                 dx al  in
                8 # al  test
                  2 L#  jz
                    end-code
                    NO-EXPAND

\ ---[ GetPal ]------------------------------------------------------

: GetPal ( c# -- r g b )
  $03C7 CPortOut        \ --
  $03C9 CPortIn         \ r
  $03C9 CPortIn         \ r g
  $03C9 CPortIn         \ r g b
;

\ ---[ SetPal ]------------------------------------------------------

: SetPal ( c# r g b -- )
  3 pick $03C8 CPortOut \ c# r g b
  rot  $03C9 CPortOut   \ c# g b
  swap $03C9 CPortOut   \ c# b
       $03C9 CPortOut   \ c#
  drop                  \ --
;

\ ---[ PalPlay ]-----------------------------------------------------
\ This procedure mucks about with our "virtual palette", then shoves
\ it to screen.  What it does is rotate the palette one 'position'

: PalPlay
  \ get color 200 values, store on stack for now
  Pal1[] 200 3 * + >R
  R@ .red C@
  R@ .green C@
  R> .blue C@
  \ copy Pal1[] to Pal3[]
  Pal1[] Pal3[] 256 3 * cmove
  \ copy Pal3[1..199] to Pal1[2..200]
  Pal3[] 3 + Pal1[] 6 + 199 3 * cmove
  \ we're leaving color 0 set to 0,0,0 so we set color 1 to the
  \ values that color 200 just had
  Pal1[] 3 + .blue C!
  Pal1[] 3 + .green C!
  Pal1[] 3 + .red C!
  WaitRetrace
  256 1 do
    i
    Pal1[] i 3 * + >R
    R@ .red C@
    R@ .green C@
    R> .blue C@
    SetPal
  loop
;

\ ---[ SetUpScreen ]-------------------------------------------------
\ This gets our screen ready but setting up the palette and drawing
\ the lines.

: SetUpScreen
  Pal1[] 256 3 * 0 fill         \ clear palette Pal1[] to zeroes
\ Initialize Pal1[] to values between 0..63
  201 0 do
\    i 64 mod Pal1[] i 3 * + .red C!
    i 64 mod Pal1[] i 3 * + .green C!
    i 64 mod Pal1[] i 3 * + .blue C!
  loop
  320 0 do
    319 199 320 i -   0 i 199 mod 1+ Line
      0   0       i 199 i 199 mod 1+ Line
    PalPlay
  loop
;

\ ---[ GrabPalette ]-------------------------------------------------
\ Copies the current palette on the VGA card into the Pal1[] array

: GrabPalette
  256 0 do
    i GetPal                    \ r g b
    Pal2[] i 3 * + >R
    R@ .blue C!                 \ r g
    R@ .green C!                \ r
    R> .red C!                  \ --
  loop
;

\ ---[ BlackOut ]----------------------------------------------------
\ This procedure blackens the screen by setting the palette values of
\ all the colors to zero.

: BlackOut ( -- )
  WaitRetrace
  256 0 do
    i 0 0 0 SetPal
  loop
;

\ ---[ HiddenScreenSetup ]-------------------------------------------
\ This procedure sets up the screen while it is blacked out, so that
\ the user can't see what is happening.

: HiddenScreenSetup
  320 0 do
    200 0 do
      j i 256 rnd FastPlot
    loop
  loop
;

\ ---[ FadeUp ]------------------------------------------------------
\ This procedure slowly fades up the new screen

: FadeUp ( -- )
  64 0 do
    WaitRetrace
    256 0 do
      i dup GetPal                       \ i r g b
      Pal2[] i 3 * + >R                  \ i r g b
    \ check the blue element
      R@ .blue C@ over > if 1+ then rot  \ i g b r
    \ check the red element
      R@ .red C@ over > if 1+ then rot   \ i b r g
    \ check the green element
      R> .green C@ over > if 1+ then rot \ i r g b
    \ update the VGA register
      SetPal                             \ --
    loop
  loop
;

\ ---[ FadeDown ]----------------------------------------------------
\ This procedure fades the screen out to black.

: FadeDown ( -- )
  64 0 do
    WaitRetrace
    256 0 do
      i dup GetPal                      \ i r g b
      dup 0 > if 1- then rot            \ i g b r
      dup 0 > if 1- then rot            \ i b r g
      dup 0 > if 1- then rot            \ i r g b
      SetPal
    loop
  loop
;

\ ---[ RestorePalette ]----------------------------------------------
\ This procedure restores the origional palette

: RestorePalette
  WaitRetrace
  256 0 do
    i
    Pal2[] i 3 * + >R
    R@ .red C@
    R@ .green C@
    R> .blue C@
    SetPal
  loop
;

: PPlay
  page
  ."      ---------------------------------------------------------------------" cr
  ."                              Palette Manipulations" cr
  ."      ---------------------------------------------------------------------" cr
  cr
  ."      This program will draw lines of different colors across the screen" cr
  ."      and change them only by changing their palette values.  The nice" cr
  ."      thing about using the palette is that one palette change changes the" cr
  ."      same color over the whole screen, without you having to redraw it." cr
  ."      Because I am using a WaitRetrace command, turning on and off your" cr
  ."      turbo during the demonstration should have no effect." cr
  cr
  ."      The second part of the demo blacks out the screen using the palette," cr
  ."      fades in the screen, waits for a keypress, then fades it out again." cr
  ."      I haven't put in any delays for the fadein/out, so you will have to" cr
  ."      put 'em in yourself to get it to the speed you like." cr
  cr
  ."      Have fun and enjoy! ;-)" cr
  cr
  ."                            Hit any key to continue ..." cr
  key 27 = not if
    FadeDown
    $13 InitGraph
    GrabPalette
    SetUpScreen
    begin
      key? not
    while
      PalPlay     \ repeat until a key is pressed
    repeat
    key drop      \ lose the keypress
    FadeDown
    Blackout
    HiddenScreenSetup
    FadeUp
    key drop
    FadeDown
    RestorePalette
    CloseGraph
    cr
    ." End of [PPLAY]" cr
    cr
    ." Enter 'pplay' to see the demo again, and again, and again...." cr
    cr
  else
    cr ." Program aborted" cr
  then
;

\ ---[ End of CLFCODE4.4TH ]-----------------------------------------

