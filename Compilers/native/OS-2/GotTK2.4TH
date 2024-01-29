\ ---[ Code Addendum 02 ]-----------------------------[02/04/2007]---
\
\          For the Graphics of the Third Kind Part II column
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
\ Save as GotTK2.4th in your \os2forth\forth directory

\ This Code Addendum is stand-alone.  Modules required from previous
\ Addendums are included.

exists [GotTK2] [if]
  forget [GotTK2]
[then]

: [GotTK2] ;

decimal

\ ---[ Library Includes from previous Columns ]----------------------

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

value VSelector $0A000 SEG>DESC to VSelector

code BlitBuffer ( &buf -- )
                                \ bx=&buf on entry as TOS
                    es  push    \ save registers to modify
                    di  push
                    si  push
        VSelector # ax  mov
                 ax es  mov     \ es=vselector
                 di di  xor     \ di=offset 0
                 bx si  mov     \ si=&buf
                        cld     \ increment si/di during rep
            16000 # cx  mov     \ cx=# of dwords to copy
                   rep  movs    \ blit it
                    si  pop
                    di  pop
                    es  pop
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

\ ---------- RANDOM NUMBERS IN FORTH

\  D. H. Lehmers Parametric multiplicative linear congruential
\  random number generator is implemented as outlined in the
\  October 1988 Communications of the ACM ( V 31 N 10 page 1192)

     16807 CONSTANT A
2147483647 CONSTANT M
    127773 CONSTANT Q   \ m a /
      2836 CONSTANT R   \ m a mod

CREATE SEED  123475689 ,

\ Returns a full cycle random number
: RAND ( -- rand )  \ 0 <= rand < 4,294,967,295
   SEED @ Q /MOD ( lo high)
   R * SWAP A * 2DUP > IF  - ELSE  - M +  THEN  DUP SEED ! ;

\ Returns single random number less than n
: RND ( n -- rnd )  \ 0 <= rnd < n
   RAND SWAP MOD ;

\ ---[ EDO ]---------------------------------------------------------
\ The Extended Data Objects package by GT Hawkins

1 CONSTANT 1BYTE
cell CONSTANT 1WORD
8 constant 1FLOAT
: BYTE* ;
: WORD+ ( n -- n+WORD )         1WORD + ;
: WORD* ( n -- n*WORD )         1WORD * ;
: DEF   ( size -- )             CONSTANT ;
: S{    ( -- 0 )                0 ;
: ::    ( offset object-definition -- offset )
  CREATE OVER , + DOES> @ + ;
: }S    ( size -- )  \ definition name follows in input stream
  CONSTANT ;
: []    ( object-definition #objects -- )
  OVER * CONSTANT                       \ define the vector
  CREATE ,                              \ define the vector operator
  DOES> @ * + ;

\ ---[ My additions to the EDO definition ]--------------------------

: FLOAT* ( n -- n*FLOAT )       FLOATS ;
  1BYTE DEF {1BYTE}-DEF
2 BYTE* DEF {2BYTE}-DEF
4 BYTE* DEF {4BYTE}-DEF
  1WORD DEF {1WORD}-DEF
2 WORD* DEF {2WORD}-DEF
8 BYTE* DEF {1FLOAT}-DEF
\ -------------------------------------------------------[End EDO]---

\ ---[ Heap Allot ]--------------------------------------------------

value %EndHeap          \ End of Heap pointer
value %HeapPtr          \ Top of Heap pointer

SP@ 65536 -             \ set end of heap to TOS-65536
                        \ change if you need more stack space
dup 16 mod -            \ align to paragraph boundary
to %EndHeap

%EndHeap to %HeapPtr    \ they are equal at initialization

: HeapAllot ( size "name" -- addr )
  create                \ =[Compile Time Functions]=
    %HeapPtr over -     \ calculate new TOS
    dup 16 mod -        \ align to lower paragraph boundary
    dup ,               \ store address of new allocation
    dup to %HeapPtr     \ update to new TOS
    swap 0 fill         \ zero the memory block
  does>                 \ =[Run Time Function]=
    @                   \ load start address of memory block
;
\ ------------------------------------------------[End Heap Allot]---

\ ---[ Type Definitions ]--------------------------------------------

{1FLOAT}-DEF 360 [] sc[]-obj sc-ndx

sc[]-obj HeapAllot Sine[]
sc[]-obj HeapAllot Cosine[]

S{
  {1WORD}-DEF :: .cX
  {1WORD}-DEF :: .cY
  {1WORD}-DEF :: .cZ
}S cube-obj

S{
  {1WORD}-DEF :: .pa
  {1WORD}-DEF :: .pb
  {1WORD}-DEF :: .pc
  {1WORD}-DEF :: .pd
  {1WORD}-DEF :: .pcolor
}S seg-obj

S{
  {1WORD}-DEF :: .obj#
  {1WORD}-DEF :: .zavg
}S zsum-obj

cube-obj 8 [] cube[]-obj cube-ndx
seg-obj  6 [] cseg[]-obj cseg-ndx
zsum-obj 6 [] zsum[]-obj zsum-ndx

\ ---[ Array Allocations ]-------------------------------------------

cube[]-obj HeapAllot Cube[]
cube[]-obj HeapAllot Temp[]
65536 HeapAllot VPage0[]        \ allocate double buffer
zsum[]-obj HeapAllot ZSum[]     \ for zbuffering
zsum-obj HeapAllot TempZSum[]   \ for swapping

\                     +-----------------+
\                    0|A               D|3
\                     | \               | \
\                     |  \              |  \
\                     |   \             |   \
\                     |    +-----------------+
\                     |   4|E           |   H|7
\                     |    |            |    |
\                     |    |      X     |    |
\                    1|B   |           C|2   |
\                     +----|------------+    |
\                      \   |             \   |
\                       \  |              \  |
\                        \ |               \ |
\                         5|F               G|6
\                          +-----------------+

\ Specify the side segments that make up the cube
\ I've added a color element value

create CSegs[]
0 , 1 , 2 , 3 ,  1 ,     \ ABCD
4 , 5 , 6 , 7 ,  2 ,     \ EFGH
0 , 1 , 5 , 4 ,  3 ,     \ ABFE
7 , 6 , 2 , 3 , 14 ,     \ HGCD
0 , 4 , 7 , 3 , 15 ,     \ AEHD
5 , 1 , 2 , 6 ,  6 ,     \ FBCG

\ ---[ Sine Tables ]-------------------------------------------------

floating

: CreateLookupTables ( -- )
  360 0 do
    i S>F 3.14159265 F* 180.0 F/ FSINCOS
    Cosine[] i sc-ndx F!
    Sine[]   i sc-ndx F!
  loop
;

integer

\ ---[ Create Vector Object ]----------------------------------------

: SetObject ( x y z obj# -- )
  cube[] swap cube-ndx >R
  R@ .cZ !
  R@ .cY !
  R> .cX !
;

: InitCube ( -- )
  -35  35 -35 0 SetObject               \ back top left coords
  -35 -35 -35 1 SetObject               \ back bottom left coords
   35 -35 -35 2 SetObject               \ back bottom right coords
   35  35 -35 3 SetObject               \ back top right coords
  -35  35  35 4 SetObject               \ front top left coords
  -35 -35  35 5 SetObject               \ front bottom left coords
   35 -35  35 6 SetObject               \ front bottom right coords
   35  35  35 7 SetObject               \ front top right coords
;

value %nx
value %ny
value %nz
value xangle
value yangle
value zangle
value distance
value direction

floating

: RotateVectors
  8 0 do
    cube[] i cube-ndx >R
    R@ .cY @ S>F Cosine[] xangle sc-ndx F@ F*
    R@ .cZ @ S>F Sine[]   xangle sc-ndx F@ F* F- F>S to %ny
    R@ .cY @ S>F Sine[]   xangle sc-ndx F@ F*
    R@ .cZ @ S>F Cosine[] xangle sc-ndx F@ F* F+ F>S to %nz
    R> .cX @ to %nx
    temp[] i cube-ndx >R
    %nx R@ .cX !
    %ny R@ .cY !
    %nz R@ .cZ !
    R@ .cX @ S>F Cosine[] yangle sc-ndx F@ F*
    R@ .cZ @ S>F Sine[]   yangle sc-ndx F@ F* F+ F>S to %nx
    R@ .cX @ negate S>F Sine[]   yangle sc-ndx F@ F*
    R@ .cZ @        S>F Cosine[] yangle sc-ndx F@ F* F+ F>S to %nz
    %nx R@ .cX !
    %nz R@ .cZ !
    R@ .cX @ S>F Cosine[] zangle sc-ndx F@ F*
    R@ .cY @ S>F Sine[]   zangle sc-ndx F@ F* F- F>S to %nx
    R@ .cX @ S>F Sine[]   zangle sc-ndx F@ F*
    R@ .cY @ S>F Cosine[] zangle sc-ndx F@ F* F+ F>S to %ny
    %nx R@ .cX !
    %ny R@ .cY !
    R@ .cZ @ distance - R@ .cZ !
    R@ .cX @ 256 * R@ .cZ @ / 160 + R@ .cX !
    R@ .cY @ 256 * R@ .cZ @ / 100 + R> .cY !
  loop
;

\ ---[ Define an X1/X2 Type ]----------------------------------------

S{
  {1WORD}-DEF :: .x1
  {1WORD}-DEF :: .x2
}S xpos-obj

\ ---[ Define the XPos[] Array ]-------------------------------------
\ The 200 defines how many vertical lines we will be working with.
\ If/when going to higher resolutions, change this.  If you will be
\ working with several different resolutions, set this to the highest
\ value you will be using (probably 1024)

xpos-obj 200 [] xpos[]-obj xpos-ndx

\ ---[ XPos[] Array ]------------------------------------------------
\ Where the points that define the left/right side of each line go.

create XPos[] here xpos[]-obj dup allot 0 fill

\ I am adding this variable, to allow the HorLine routine to draw
\ the polygon data either to a double buffer or directly to the
\ video screen.

variable UseBuffer 1 UseBuffer !

\ ---[ HorLine ]-----------------------------------------------------
\ This draws to the specified double buffer
\ If you set UseBuffer to 0 to draw to the screen, then you MUST
\ pass a 0 value for the &buf address.  This is added to the target
\ DI offset value, and MUST be present.

code (HorLine) ( &buf x1 x2 y c -- )
                                \ c in bx on entry as TOS
                    bx  push    \ put all parameters onto the stack
                 bp sp  xchg    \ setup the stack frame
                    es  push    \ save registers to be used
                    di  push
                    ds  push
                    es  pop
       UseBuffer #) ax  mov
                0 # ax  cmp
                  1 L#  jne     \ if !0, leave ES=DS
        VSelector # ax  mov     \ else, draw direct to screen
                 ax es  mov
1 L:

       3 cells [bp] ax  mov
       2 cells [bp] ax  cmp
                  2 L#  jl      \ if x1<x2 continue
       2 cells [bp] bx  mov     \ else swap them
       ax 2 cells [bp]  mov
       bx 3 cells [bp]  mov
2 L:
       \ Calculate Y*320+X
       3 cells [bp] di  mov     \ di=x1
       1 cells [bp] ax  mov     \ ax=y
                6 # cl  mov
                 ax cl  shl     \ ax=y1*64
                 ax di  add     \ di=y*64+x
                2 # cl  mov
                 ax cl  shl     \ ax=y*256
                 ax di  add     \ di=y*320+x
       4 cells [bp] di  add     \ di=&buf[y*320+x]
       \ Calculate Length
       2 cells [bp] ax  mov     \ ax=x2
       3 cells [bp] ax  sub     \ ax=x2-x1
                    ax  inc     \ ax=x2-x1+1
                 ax dx  mov     \ dx=len
       0 cells [bp] al  mov     \ al=c
                 al ah  mov     \ put c into all 4 bytes of EAX
               16 # cl  mov
                 ax cl  shl
       0 cells [bp] al  mov
                 al ah  mov
                 dx cx  mov
\ This handles the initial bytes of the line, so that the rest
\ can be plotted using the STOS opcode, 4 bytes/rep
\ This handles line lengths less than 4 pixels correctly.
                        cld     \ set forward direction for DI inc
                    cx  push
                3 # cx  and     \ get bits 0/1
                0 # cx  cmp
                  3 L#  je      \ if=0 then no extra pixels to plot
                        rep     \ else plot up to 3 pixels now
                    al  stosb
3 L:
                    cx  pop     \ retrieve count
                    cx  shr     \ cx=cx>>1
                    cx  shr     \ cx=cx>>2 (divide by 4)
                0 # cx  cmp     \ if cx=0, then exit
                  4 L#  je
                        rep
                    ax  stos    \ and store the dwords left
4 L:
                    di  pop     \ restore register
                    es  pop
                 bp sp  xchg    \ restore stack frame
          5 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

\ ---[ Polygon ]-----------------------------------------------------
\ I am putting all the passed parameters into VALUEs to make it an
\ easier task to visualize what the code is doing

value p.buf
value p.x1
value p.y1
value p.x2
value p.y2
value p.x3
value p.y3
value p.x4
value p.y4
value p.c

\ Local variables

value p.mny
value p.mxy
value p.y
value p.s1
value p.s2
value p.s3
value p.s4
value p.pos

\ boolean selector function

: ?pos  ( &XPos[y] 0/1 -- &XPos[y].x1/x2 )
  case
    0 of .x1 endof
    1 of .x2 endof
  endcase
;

\ ---[ ScanEdge ]----------------------------------------------------
\ This creates a Horizontal Line List of the line being processed.
\ The line from (xl,yl) to (xh,yh) is checked using the Bresenham
\ Line algorithm, and the X coordinate is placed into the XPos[]
\ array at the Yth element of the array.
\ The Left or Right edge question is handled by the p.pos value, that
\ is a boolean set by the yl<yh test: if yl<yh, we are working with
\ the x1 coordinates, else the x2 coordinates of the horizontal line
\ being worked on.

value se.x1
value se.y1
value se.xh
value se.yh
value se.s
value se.y
value se.pos

: ScanEdge ( xl yl xh yh s -- )
  to se.s
  to se.yh to se.xh
  to se.y1 to se.x1
  se.y1 to se.y
  se.y1 se.yh < abs to se.pos
  se.y1 se.yh <> if
    begin
      se.xh se.x1 -     \ xh-x1
      se.y se.y1 -      \ xh-x1 y-y1
      *                 \ xh-x1*y-y1
      se.yh se.y1 -     \ xh-x1*y-y1 yh-y1
      /                 \ xh-x1*y-y1/yh-y1
      se.x1 +           \ xh-x1*y-y1/yh-y1+x1
      XPos[]            \ xh-x1*y-y1/yh-y1+x1 &XP[]
      se.y              \ xh-x1*y-y1/yh-y1+x1 &XP[] y
      xpos-ndx          \ xh-x1*y-y1/yh-y1+x1 &XP[y]
      se.pos ?pos       \ xh-x1*y-y1/yh-y1+x1 &XP[y].x?
      !                 \ --
      se.s +to se.y     \
      se.yh se.s +      \ yh+s
      se.y =            \ 0/1
    until
  else
    se.x1               \ x1
    XPos[]              \ x1 &XP[]
    se.y                \ x1 &XP[] y
    xpos-ndx            \ x1 &XP[y]
    se.pos ?pos         \ x1 &XP[y].x?
    !                   \ --
  then
;

: polygon ( &buf x1 y1 x2 y2 x3 y3 x4 y4 c -- )
  to p.c
  to p.y4 to p.x4
  to p.y3 to p.x3
  to p.y2 to p.x2
  to p.y1 to p.x1
  to p.buf

  \ sort the points based on the y values
  p.y1 to p.mny
  p.y2 p.mny < if p.y2 to p.mny then
  p.y3 p.mny < if p.y3 to p.mny then
  p.y4 p.mny < if p.y4 to p.mny then

  p.y1 to p.mxy
  p.y2 p.mxy > if p.y2 to p.mxy then
  p.y3 p.mxy > if p.y3 to p.mxy then
  p.y4 p.mxy > if p.y4 to p.mxy then

  p.y1 p.y2 < abs 2* 1- to p.s1
  p.y2 p.y3 < abs 2* 1- to p.s2
  p.y3 p.y4 < abs 2* 1- to p.s3
  p.y4 p.y1 < abs 2* 1- to p.s4

\ Process line segment x1,y1 to x2,y2
  p.x1 p.y1 p.x2 p.y2 p.s1 ScanEdge

\ Process line segment x2,y2 to x3,y3
  p.x2 p.y2 p.x3 p.y3 p.s2 ScanEdge

\ Process line segment x3,y3 to x4,y4
  p.x3 p.y3 p.x4 p.y4 p.s3 ScanEdge

\ Process line segment x4,y4 to x1,y1
  p.x4 p.y4 p.x1 p.y1 p.s4 ScanEdge

  p.mxy 1+ p.mny do
    p.buf
    XPos[] i xpos-ndx .x1 @
    XPos[] i xpos-ndx .x2 @
    i
    p.c
    (HorLine)
  loop
;

\ ---[ Raw Polygon Speed Demo ]--------------------------------------

: FillPoly1
  $13 InitGraph
  1 UseBuffer !
  begin
    key? not
  while
    VPage0[]            \ &buf
    300 rnd 10 +        \ &buf x1
    180 rnd 10 +        \ &buf x1 y1
    300 rnd 10 +        \ &buf x1 y1 x2
    180 rnd 10 +        \ &buf x1 y1 x2 y2
    300 rnd 10 +        \ &buf x1 y1 x2 y2 x3
    180 rnd 10 +        \ &buf x1 y1 x2 y2 x3 y3
    300 rnd 10 +        \ &buf x1 y1 x2 y2 x3 y3 x4
    180 rnd 10 +        \ &buf x1 y1 x2 y2 x3 y3 x4 y4
    256 rnd             \ &buf x1 y1 x2 y2 x3 y3 x4 y4 c
    Polygon             \ --
    VPage0[] BlitBuffer
  repeat
  key drop
  CloseGraph
;

: FillPoly2
  $13 InitGraph
  0 UseBuffer !
  begin
    key? not
  while
    0                   \ 0
    300 rnd 10 +        \ 0 x1
    180 rnd 10 +        \ 0 x1 y1
    300 rnd 10 +        \ 0 x1 y1 x2
    180 rnd 10 +        \ 0 x1 y1 x2 y2
    300 rnd 10 +        \ 0 x1 y1 x2 y2 x3
    180 rnd 10 +        \ 0 x1 y1 x2 y2 x3 y3
    300 rnd 10 +        \ 0 x1 y1 x2 y2 x3 y3 x4
    180 rnd 10 +        \ 0 x1 y1 x2 y2 x3 y3 x4 y4
    256 rnd             \ 0 x1 y1 x2 y2 x3 y3 x4 y4 c
    Polygon             \ --
  repeat
  key drop
  CloseGraph
;

\ ---[ Filled Polygon - Non Sorted ]---------------------------------

: DrawCube ( -- )
  RotateVectors
  6 0 do
    VPage0[]                    \ draw image to buffer
    CSegs[] i cseg-ndx >R
    temp[] R@ .pa @ cube-ndx dup .cX @ swap .cY @
    temp[] R@ .pb @ cube-ndx dup .cX @ swap .cY @
    temp[] R@ .pc @ cube-ndx dup .cX @ swap .cY @
    temp[] R@ .pd @ cube-ndx dup .cX @ swap .cY @
    R> .pcolor @
    Polygon
  loop
  WaitRetrace                   \ delay
  VPage0[] BlitBuffer           \ display buffer data
  VPage0[] 65536 0 fill
;

: fcube1 ( -- )
   25 to xangle           \ init variables and arrays
  125 to yangle
  275 to zangle
  200 to distance
    1 to UseBuffer

  VPage0[] 64000 0 fill
  CreateLookupTables      \ init the sine/cosine tables
  InitCube                \ init the cube[] structure
  $13 InitGraph
  begin                   \ repeat until a key is pressed
    key? not
  while
    \ update the angles
    3 +to xangle  xangle 359 > if 0 to xangle then
    3 +to yangle  yangle 359 > if 0 to yangle then
    -2 +to zangle zangle 1 < if 359 to zangle then
    DrawCube
  repeat
  key drop
  CloseGraph
;

\ ---[ Filled Polygon - Sorted ]-------------------------------------

: ZAverage
  6 0 do
    i ZSum[] i zsum-ndx .obj# !
  loop
  \ Calc zaverage for side ABCD
  temp[] 0 cube-ndx .cZ @
  temp[] 1 cube-ndx .cZ @ +
  temp[] 2 cube-ndx .cZ @ +
  temp[] 3 cube-ndx .cZ @ + 4 / ZSum[] 0 zsum-ndx .zavg !
  \ Calc zaverage for side EFGH
  temp[] 4 cube-ndx .cZ @
  temp[] 5 cube-ndx .cZ @ +
  temp[] 6 cube-ndx .cZ @ +
  temp[] 7 cube-ndx .cZ @ + 4 / ZSum[] 1 zsum-ndx .zavg !
  \ Calc zaverage for side ABFE
  temp[] 0 cube-ndx .cZ @
  temp[] 1 cube-ndx .cZ @ +
  temp[] 5 cube-ndx .cZ @ +
  temp[] 4 cube-ndx .cZ @ + 4 / ZSum[] 2 zsum-ndx .zavg !
  \ Calc zaverage for side DCGH
  temp[] 3 cube-ndx .cZ @
  temp[] 2 cube-ndx .cZ @ +
  temp[] 6 cube-ndx .cZ @ +
  temp[] 7 cube-ndx .cZ @ + 4 / ZSum[] 3 zsum-ndx .zavg !
  \ Calc zaverage for side AEHD
  temp[] 0 cube-ndx .cZ @
  temp[] 4 cube-ndx .cZ @ +
  temp[] 7 cube-ndx .cZ @ +
  temp[] 3 cube-ndx .cZ @ + 4 / ZSum[] 4 zsum-ndx .zavg !
  \ Calc zaverage for side BFGC
  temp[] 1 cube-ndx .cZ @
  temp[] 5 cube-ndx .cZ @ +
  temp[] 6 cube-ndx .cZ @ +
  temp[] 2 cube-ndx .cZ @ + 4 / ZSum[] 5 zsum-ndx .zavg !
;

\ ---[ ZBuffering ]--------------------------------------------------
\ Sorts the image structures based on the .cZ value of each.

: ZBuffering ( -- )
  ZAverage
  6 1- 0 do
    6 i 1+ do
      ZSum[] j zsum-ndx .zavg @
      ZSum[] i zsum-ndx .zavg @ >= if
        \ swap the two structures
        ZSum[] j zsum-ndx TempZSum[] zsum-obj cmove
        ZSum[] i zsum-ndx ZSum[] j zsum-ndx zsum-obj cmove
        TempZSum[] ZSum[] i zsum-ndx zsum-obj cmove
      then
    loop
  loop
;

: DrawCube1
  RotateVectors
  ZBuffering
  VPage0[] 65536 0 fill
  6 3 do
    VPage0[]
    ZSum[] i zsum-ndx .obj# @
    CSegs[] swap cseg-ndx >R
    temp[] R@ .pa @ cube-ndx dup .cX @ swap .cY @
    temp[] R@ .pb @ cube-ndx dup .cX @ swap .cY @
    temp[] R@ .pc @ cube-ndx dup .cX @ swap .cY @
    temp[] R@ .pd @ cube-ndx dup .cX @ swap .cY @
    R> .pcolor @
    Polygon
  loop
;

: fcube2 ( -- )
   25 to xangle                 \ init variables and arrays
  125 to yangle
  275 to zangle
  200 to distance
    1 to direction
    1 to UseBuffer

  CreateLookupTables            \ init the sine/cosine tables
  InitCube                      \ init the cube[] structure
  $13 InitGraph
  begin                         \ repeat until a key is pressed
    key? not
  while
    direction 1 = if            \ update distance & direction
      1 +to distance
      distance 599 > if 0 to direction 600 to distance then
    else
      -1 +to distance
      distance 200 < if 1 to direction 200 to distance then
    then
  \ update the angles
    3 +to xangle  xangle 359 > if 0 to xangle then
    3 +to yangle  yangle 359 > if 0 to yangle then
    -2 +to zangle zangle 1 < if 359 to zangle then

    DrawCube1
    WaitRetrace
    VPage0[] BlitBuffer
  repeat
  key drop
  CloseGraph
;

cr .( FillPoly1 FillPoly2 fcube1 fcube2) cr


