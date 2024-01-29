\ ---[ Code Addendum 02 ]-----------------------------[12/12/2006]---
\
\          For the Graphics of the Second Kind Part II column
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
\ Save as GotSK2.4th in your \os2forth\forth directory

exists [GotSK2] [if]
  forget [GotSK2]
[then]

: [GotSK2] ;

\ ---[ Library Routines ]--------------------------------------------
\ I'm adding the various code modules from the previous columns that
\ are needed for this column.

value VSelector $0A000 SEG>DESC to VSelector

code SetMode ( mode -- )
                                \ bx=mode on entry as TOS
                 ax ax  xor     \ ah=0 (function #0)
                 bl al  mov     \ al=mode
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

     16807 CONSTANT A
2147483647 CONSTANT M
    127773 CONSTANT Q   \ m a /
      2836 CONSTANT R   \ m a mod

create %SEED 123475689 ,

: RAND ( -- rand )  \ 0 <= rand < 4,294,967,295
   %SEED @ Q /MOD ( lo high)
   R * SWAP A * 2DUP > IF  - ELSE  - M +  THEN  DUP %SEED ! ;

: RND ( n -- rnd )  \ 0 <= rnd < n
   RAND SWAP MOD ;

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

: GetPal ( c# -- r g b )
  $03C7 CPortOut        \ --
  $03C9 CPortIn         \ r
  $03C9 CPortIn         \ r g
  $03C9 CPortIn         \ r g b
;

: SetPal ( c# r g b -- )
  3 pick $03C8 CPortOut \ c# r g b
  rot  $03C9 CPortOut   \ c# g b
  swap $03C9 CPortOut   \ c# b
       $03C9 CPortOut   \ c#
  drop                  \ --
;

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

\ ---[ Sprite Code for the GotSK2 Column ]---------------------------

binary
create Square[]
11111111 C, 11111111 C,
11111111 C, 11111111 C,
11111111 C, 11111111 C,
11111111 C, 11111111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11111111 C, 11111111 C,
11111111 C, 11111111 C,
11111111 C, 11111111 C,
11111111 C, 11111111 C,

create Diamond[]
00000001 C, 10000000 C,
00000011 C, 11000000 C,
00000110 C, 01100000 C,
00001100 C, 00110000 C,
00011001 C, 10011000 C,
00110011 C, 11001100 C,
01100110 C, 01100110 C,
11001100 C, 00110011 C,
11001100 C, 00110011 C,
01100110 C, 01100110 C,
00110011 C, 11001100 C,
00011001 C, 10011000 C,
00001100 C, 00110000 C,
00000110 C, 01100000 C,
00000011 C, 11000000 C,
00000001 C, 10000000 C,

create Ovoid[]
00001111 C, 11110000 C,
00111111 C, 11111100 C,
01111111 C, 11111110 C,
01111111 C, 11111110 C,
11111000 C, 00011111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11110000 C, 00001111 C,
11111000 C, 00011111 C,
01111111 C, 11111110 C,
01111111 C, 11111110 C,
00111111 C, 11111100 C,
00001111 C, 11110000 C,

decimal

value %UseVR
value %Speed

value %src
value %dst

: DrawPackedSprite ( &image x y c -- )
  >R rot to %src R>             \ x y c
  16 0 do                       \ x y c
    %src C@ 1 +to %src          \ x y c chi
    8 LSHIFT                    \ x y c chi<<8
    %src C@ 1 +to %src          \ x y c chi<<8 clo
    OR                          \ x y c n
    16 0 do                     \ x y c n
      dup $8000 and if          \ x y c n
        3 pick i +              \ x y c n x+i
        3 pick j +              \ x y c n x+i y+j
        3 pick                  \ x y c n x+i y+j c
        %Speed if
          FastPlot
        else
          Plot
        then
      then
      1 LSHIFT
    loop
    drop
  loop
  2drop
  drop
;

code Fast.DrawPackedSprite ( &image x y c -- )
                    bx  push    \ all parms to stack
                 bp sp  xchg    \ setup stack frame
                    es  push    \ save registers to modify
                    di  push
        VSelector # ax  mov
                 ax es  mov
                 di di  xor
       3 cells [bp] bx  mov     \ bx=&image
       2 cells [bp] di  add     \ di=&buf[x]
       1 cells [bp] ax  mov     \ ax=y
                6 # cl  mov
                 ax cl  shl     \ ax=y*64
                 ax di  add     \ di=&buf[y*64+x]
                2 # cl  mov
                 ax cl  shl     \ ax=y*64
                 ax di  add     \ di=&buf[y*320+x]
               16 # cx  mov     \ height loop
1 L:
                    cx  push
             0 [bx] ah  mov
                    bx  inc
             0 [bx] al  mov
                    bx  inc
               16 # cx  mov
2 L:
            $8000 # ax  test
                  3 L#  jz      \ skip if 0
                    ax  push    \ else plot it
       0 cells [bp] al  mov
         es: al 0 [di]  mov
                    ax  pop
3 L:
                    ax  shl     \ move to next bit
                    di  inc     \ increment plotted or not
                    cx  dec
                  2 L#  jnz     \ next width loop
              304 # di  add     \ advance to start of next line
                    cx  pop
                    cx  dec
                  1 L#  jnz     \ next height loop

                    di  pop     \ restore parameters
                    es  pop
                 bp sp  xchg    \ restore stack frame
          4 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

\ ---[ KeyCheck ]----------------------------------------------------
\ Monitors the keyboard, and filters specific keypresses.
\
\ If the BackSpace is pressed, the %Speed variable is toggled to
\ select between using the FastPlot and Plot routines to draw the
\ pixels to the screen.
\
\ If the Space Bar is pressed, it toggles the %UseVR variable to
\ select between using WaitRetrace to introduce a slowdown between
\ drawing cycles, or not.
\
\ Both TestSprite routines start off with WaitRetrace enabled, and
\ using the slow BIOS Plot routine enabled.
\
\ Any other key exits the demo word
\ -------------------------------------------------------------------

: KeyCheck ( -- 0/1 )
  key? if
    key dup 32 = if             \ Space Bar toggles using Retrace
      drop
      %UseVR not to %UseVR
      0
    else
      8 = if                    \ Backspace toggles Plot/FastPlot
        %Speed not to %Speed
        0
      else
        1
      then
    then
  else
    0
  then
  not                           \ reverse it for the While check
;

\ ---[ TestSprite1 ]-------------------------------------------------
\ Draws a matrix of 16 sprites across by 11 sprites down the screen.
\ It then draws at random one of the three defined sprites in random
\ colors at each position.
\ Reference the description of KeyCheck above for the key presses.

: TestSprite1
  $13 InitGraph
  1 to %UseVR           \ start using WaitRetrace
  0 to %Speed           \ start using BIOS Plot
  begin
    KeyCheck
  while
    11 0 do
      16 0 do
        3 rnd
        case
          0 of Square[]  endof
          1 of Diamond[] endof
          2 of Ovoid[]   endof
        endcase
        i 20 *
        j 18 *
        256 rnd
        DrawPackedSprite
      loop
    loop
    %UseVR if WaitRetrace then
  repeat
  FadeDown
  CloseGraph
;

\ ---[ TestSprite2 ]-------------------------------------------------
\ Draws one of the three defined sprites in a randomly selected color
\ at random x,y coordinates all over the screen.
\ Again, reference the KeyCheck description for the key presses.
\
\ This one draws 1,000,000 of these sprites in 30.32 seconds on my
\ system.

: TestSprite2
  1 to %UseVR           \ start using WaitRetrace
  0 to %Speed           \ start using BIOS Plot
  $13 InitGraph
  begin
    KeyCheck
  while
    3 rnd
    case
      0 of Square[]  endof
      1 of Diamond[] endof
      2 of Ovoid[]   endof
    endcase
    320 16 - rnd
    200 16 - rnd
    256 rnd
    %Speed if
      Fast.DrawPackedSprite
    else
      DrawPackedSprite
    then
    %UseVR if WaitRetrace then
  repeat
  FadeDown
  CloseGraph
;

cr .( GotSK2: TestSprite1 TestSprite2 demo words)


create BollPalette[]
\ 24*3 bytes
         0 C,  0 C,  0 C, \ color # 0
         0 C,  0 C, 12 C, \ color # 1
         4 C,  4 C, 12 C, \ color # 2
         4 C,  4 C, 16 C, \ color # 3
         4 C,  4 C, 20 C, \ color # 4
         4 C,  4 C, 24 C, \ color # 5
         8 C,  8 C, 24 C, \ color # 6
         8 C,  8 C, 28 C, \ color # 7
         8 C,  8 C, 32 C, \ color # 8
         8 C,  8 C, 36 C, \ color # 9
        12 C, 12 C, 40 C, \ color # 10
        12 C, 12 C, 44 C, \ color # 11
        12 C, 12 C, 48 C, \ color # 12
        12 C, 12 C, 52 C, \ color # 13
        16 C, 16 C, 48 C, \ color # 14
        16 C, 16 C, 52 C, \ color # 15
        16 C, 16 C, 56 C, \ color # 16
        20 C, 20 C, 56 C, \ color # 17
        20 C, 20 C, 60 C, \ color # 18
        24 C, 24 C, 60 C, \ color # 19
        28 C, 28 C, 60 C, \ color # 20
        32 C, 32 C, 60 C, \ color # 21
        36 C, 36 C, 60 C, \ color # 22
        60 C, 60 C, 60 C, \ color # 23

create Boll[]
31 , 24 ,       \ height, width
 0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  8 C,  9 C, 10 C, 10 C, 10 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  8 C,
 9 C, 10 C, 11 C, 11 C, 12 C, 12 C, 12 C, 11 C,
 0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  0 C,  5 C,  8 C,  9 C,
10 C, 11 C, 11 C, 12 C, 12 C, 13 C, 13 C, 12 C,
12 C, 10 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  5 C,  7 C,  9 C, 10 C,
11 C, 11 C, 12 C, 12 C, 13 C, 15 C, 15 C, 15 C,
15 C, 12 C, 11 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  4 C,  7 C,  8 C,  9 C, 10 C,
11 C, 12 C, 12 C, 13 C, 15 C, 15 C, 16 C, 16 C,
15 C, 15 C, 12 C, 10 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  5 C,  7 C,  8 C,  9 C, 10 C,
11 C, 12 C, 12 C, 13 C, 15 C, 16 C, 16 C, 16 C,
16 C, 15 C, 13 C, 12 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  4 C,  6 C,  8 C,  9 C, 10 C, 10 C,
11 C, 12 C, 12 C, 13 C, 15 C, 16 C, 16 C, 16 C,
16 C, 16 C, 15 C, 12 C, 11 C,  0 C,  0 C,  0 C,
 0 C,  2 C,  4 C,  7 C,  8 C,  9 C, 10 C, 10 C,
11 C, 12 C, 12 C, 13 C, 15 C, 15 C, 16 C, 16 C,
16 C, 16 C, 15 C, 13 C, 12 C,  8 C,  0 C,  0 C,
 0 C,  3 C,  5 C,  7 C,  8 C,  9 C, 10 C, 10 C,
11 C, 12 C, 12 C, 13 C, 15 C, 15 C, 16 C, 16 C,
16 C, 16 C, 15 C, 13 C, 12 C, 10 C,  0 C,  0 C,
 0 C,  3 C,  5 C,  7 C,  8 C,  9 C,  9 C, 10 C,
11 C, 11 C, 12 C, 12 C, 15 C, 16 C, 16 C, 16 C,
16 C, 15 C, 15 C, 13 C, 12 C, 11 C,  0 C,  0 C,
 2 C,  4 C,  5 C,  7 C,  8 C,  9 C,  9 C, 10 C,
11 C, 11 C, 12 C, 12 C, 19 C, 22 C, 18 C, 15 C,
15 C, 15 C, 15 C, 13 C, 12 C, 11 C,  8 C,  0 C,
 2 C,  4 C,  5 C,  7 C,  8 C,  8 C,  9 C, 10 C,
10 C, 11 C, 11 C, 14 C, 22 C, 23 C, 22 C, 15 C,
15 C, 15 C, 13 C, 12 C, 12 C, 11 C,  9 C,  0 C,
 2 C,  4 C,  5 C,  6 C,  7 C,  8 C,  9 C,  9 C,
10 C, 11 C, 11 C, 12 C, 21 C, 23 C, 22 C, 15 C,
13 C, 13 C, 12 C, 12 C, 12 C, 11 C,  9 C,  0 C,
 2 C,  3 C,  4 C,  6 C,  7 C,  8 C,  9 C,  9 C,
10 C, 10 C, 11 C, 11 C, 15 C, 20 C, 17 C, 12 C,
12 C, 12 C, 12 C, 12 C, 11 C, 10 C,  9 C,  0 C,
 2 C,  3 C,  4 C,  5 C,  7 C,  8 C,  8 C,  9 C,
 9 C, 10 C, 10 C, 11 C, 11 C, 12 C, 12 C, 12 C,
12 C, 12 C, 12 C, 11 C, 11 C, 10 C,  9 C,  0 C,
 2 C,  3 C,  4 C,  5 C,  7 C,  7 C,  8 C,  8 C,
 9 C, 10 C, 10 C, 10 C, 11 C, 11 C, 11 C, 11 C,
11 C, 11 C, 11 C, 11 C, 10 C, 10 C,  8 C,  0 C,
 2 C,  3 C,  4 C,  4 C,  6 C,  7 C,  8 C,  8 C,
 9 C,  9 C, 10 C, 10 C, 10 C, 11 C, 11 C, 11 C,
11 C, 11 C, 11 C, 10 C, 10 C,  9 C,  8 C,  0 C,
 2 C,  3 C,  3 C,  4 C,  5 C,  7 C,  7 C,  8 C,
 8 C,  9 C,  9 C,  9 C, 10 C, 10 C, 10 C, 10 C,
10 C, 10 C, 10 C, 10 C,  9 C,  9 C,  7 C,  0 C,
 2 C,  2 C,  3 C,  4 C,  5 C,  5 C,  7 C,  7 C,
 8 C,  8 C,  9 C,  9 C,  9 C, 10 C, 10 C, 10 C,
10 C, 10 C, 10 C,  9 C,  9 C,  8 C,  7 C,  0 C,
 1 C,  2 C,  3 C,  4 C,  4 C,  5 C,  6 C,  7 C,
 7 C,  8 C,  8 C,  9 C,  9 C,  9 C,  9 C,  9 C,
 9 C,  9 C,  9 C,  9 C,  8 C,  7 C,  5 C,  0 C,
 1 C,  2 C,  3 C,  3 C,  4 C,  4 C,  5 C,  6 C,
 7 C,  7 C,  8 C,  8 C,  8 C,  8 C,  9 C,  9 C,
 9 C,  9 C,  8 C,  8 C,  8 C,  7 C,  4 C,  0 C,
 1 C,  2 C,  2 C,  3 C,  3 C,  4 C,  4 C,  5 C,
 6 C,  7 C,  7 C,  7 C,  8 C,  8 C,  8 C,  8 C,
 8 C,  8 C,  8 C,  7 C,  7 C,  5 C,  3 C,  0 C,
 0 C,  1 C,  2 C,  3 C,  3 C,  4 C,  4 C,  4 C,
 5 C,  6 C,  7 C,  7 C,  7 C,  7 C,  7 C,  8 C,
 8 C,  7 C,  7 C,  7 C,  6 C,  4 C,  0 C,  0 C,
 0 C,  1 C,  2 C,  2 C,  3 C,  3 C,  4 C,  4 C,
 4 C,  5 C,  5 C,  6 C,  7 C,  7 C,  7 C,  7 C,
 7 C,  7 C,  6 C,  5 C,  4 C,  3 C,  0 C,  0 C,
 0 C,  1 C,  1 C,  2 C,  2 C,  3 C,  3 C,  4 C,
 4 C,  4 C,  5 C,  5 C,  5 C,  5 C,  6 C,  6 C,
 6 C,  5 C,  5 C,  4 C,  4 C,  2 C,  0 C,  0 C,
 0 C,  0 C,  1 C,  1 C,  2 C,  2 C,  3 C,  3 C,
 3 C,  4 C,  4 C,  4 C,  4 C,  5 C,  5 C,  5 C,
 5 C,  4 C,  4 C,  4 C,  3 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  1 C,  1 C,  2 C,  2 C,  3 C,
 3 C,  3 C,  3 C,  4 C,  4 C,  4 C,  4 C,  4 C,
 4 C,  4 C,  3 C,  3 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  1 C,  1 C,  1 C,  2 C,  2 C,
 2 C,  3 C,  3 C,  3 C,  3 C,  3 C,  3 C,  3 C,
 3 C,  3 C,  3 C,  2 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  1 C,  1 C,  1 C,  2 C,
 2 C,  2 C,  2 C,  3 C,  3 C,  3 C,  3 C,  3 C,
 3 C,  2 C,  2 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  0 C,  1 C,  1 C,  1 C,
 1 C,  2 C,  2 C,  2 C,  2 C,  2 C,  2 C,  2 C,
 2 C,  1 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,
 1 C,  1 C,  1 C,  1 C,  1 C,  1 C,  1 C,  0 C,
 0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,  0 C,

\ Simulate structure access

: .height 0 + ;                 \ element offsets
: .width  4 + ;
: .image  8 + ;

value %imagedata

: DrawSprite ( &obj x y -- )
  2 pick .image to %imagedata   \ &obj x y
  2 pick .height @ 0 do         \ &obj x y
    2 pick .width @ 0 do        \ &obj x y
      %imagedata C@             \ &obj x y c
      1 +to %imagedata          \ &obj x y c
      dup if                    \ &obj x y c
        2 pick i +              \ &obj x y c x+i
        2 pick j +              \ &obj x y c x+i y+j
        rot                     \ &obj x y x+i y+j c
        %Speed if               \ &obj x y x+i y+j c
          FastPlot              \ &obj x y
        else                    \ &obj x y x+i y+j c
          Plot                  \ &obj x y
        then                    \ &obj x y
      else                      \ &obj x y c
        drop                    \ &obj x y
      then                      \ &obj x y
    loop                        \ &obj x y
  loop                          \ &obj x y
  2drop drop                    \ --
;


: SetBollPalette
  0 $03C8 CPortOut
  24 3 * 0 do
    BollPalette[] i + C@ $03C9 CPortOut
  loop
;

: DrawBolls
  $13 InitGraph
  0 to %UseVR           \ start using WaitRetrace
  SetBollPalette
  4 0 do
    6 0 do
      Boll[] i 50 * j 50 * DrawSprite
    loop
  loop
  key drop
  FadeDown
  CloseGraph
;

: DrawRandomBolls
  1 to %UseVR           \ start using WaitRetrace
  0 to %Speed           \ start using Plot
  $13 InitGraph
  SetBollPalette
  begin
    KeyCheck
  while
    Boll[]                      \ &Boll[]
    320 over .width @ - rnd     \ &Boll[] rnd(x)
    200 2 pick .height @ - rnd  \ &Boll[] rnd(x) rnd(y)
    DrawSprite
    %UseVR if WaitRetrace then
  repeat
  FadeDown
  CloseGraph
;

cr .( GotSK2: DrawBolls DrawRandomBolls )
