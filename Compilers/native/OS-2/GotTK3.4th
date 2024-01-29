\ ---[ Code Addendum 03 ]-----------------------------[03/01/2007]---
\
\         For the Graphics of the Third Kind Part III column
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
\ Save as GotTK3.4th in your \os2forth\forth directory

\ This Code Addendum is stand-alone.  Modules required from previous
\ Addendums are included.

exists [GotTK3] [if]
  forget [GotTK3]
[then]

: [GotTK3] ;

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

code CPortOut ( c addr -- )
                                \ addr in bx on entry
                 bx dx mov      \ addr to dx
                    ax pop      \ c to ax (al, specifically)
                       $EE C,   \ OUT DX,AL
                    bx pop      \ get new TOS
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

\ ---[ GPoly3 ]------------------------------------------------------
\ A triangle polygon fill routine
\ Requires a double buffer address, three x/y coordinates, and three
\ color values to utilize.
\
\            gpoly3 ( &buf x1 y1 x2 y2 x3 y3 c1 c2 c3 -- )
\

variable %Delta

code GHLine ( &buf x1 x2 y c1 c2 -- )
                    bx  push    \ all parms to stack
                 bp sp  xchg    \ setup stack frame
       5 cells [bp] bx  mov     \ bx=&buf
       4 cells [bp] ax  mov
       3 cells [bp] ax  cmp
                  1 L#  jl      \ continue if x1<x2
       3 cells [bp] dx  mov     \ else swap x1/x2
       ax 3 cells [bp]  mov
       dx 4 cells [bp]  mov
       1 cells [bp] ax  mov     \ and swap c1/c2
       0 cells [bp] dx  mov
       ax 0 cells [bp]  mov
       dx 1 cells [bp]  mov
1 L:
\ Calculate the horizontal stepvalue per scanline

\ calculate y*320
       2 cells [bp] ax  mov     \ ax=y
                6 # cl  mov
                 ax cl  shl     \ ax=y*64
                 ax bx  add     \ bx=&buf[y*64]
                2 # cl  mov
                 ax cl  shl     \ ax=y*256
                 ax bx  add     \ bx=&buf[y*320]
       4 cells [bp] bx  add     \ bx=&buf[y*320+x1]

       1 cells [bp] ax  mov     \ ax=c1
                8 # cl  mov
                 ax cl  shl     \ c1=c1<<8 (color)
       3 cells [bp] dx  mov     \ dx=x2
       4 cells [bp] dx  sub     \ dx=x2-x1 (width)
                    dx  inc     \ may need to remove this...
                        cld
2 L:
             ah 0 [bx]  mov     \ actually plotting color>>8
                    bx  inc
          %Delta #) ax  add     \ inc(color,delta)
                    dx  dec
                  2 L#  jnz
                 bp sp  xchg    \ restore stack frame
          6 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

{1WORD}-DEF 200 [] pel[]-obj pel-ndx

pel[]-obj HeapAllot LeftX[]
pel[]-obj HeapAllot RightX[]
pel[]-obj HeapAllot LColor[]
pel[]-obj HeapAllot RColor[]

value gx1
value gx2
value gx3
value gy1
value gy2
value gy3
value gc1
value gc2
value gc3
value gbuf

value deltax_l1
value deltax_l2
value deltax_r
value delta_c1
value delta_c2
value delta_c3
value startx
value startc
value qx
value qc

: gpoly3 ( &buf x1 y1 x2 y2 x3 y3 c1 c2 c3 -- )
  to gc3 to gc2 to gc1
  to gy3 to gx3
  to gy2 to gx2
  to gy1 to gx1
  to gbuf

  gy1 gy3 > if            \ y3 must be the largest y-value
    gy3 gy1 to gy3 to gy1
    gx3 gx1 to gx3 to gx1
    gc3 gc1 to gc3 to gc1
  then

  gy1 gy2 > if
    gy2 gy1 to gy2 to gy1 \ y1 must be the smallest y-value
    gx2 gx1 to gx2 to gx1
    gc2 gc1 to gc2 to gc1
  then

  gy2 gy3 > if
    gy2 gy3 to gy2 to gy3 \ y2 must be the middle value
    gx2 gx3 to gx2 to gx3
    gc2 gc3 to gc2 to gc3
  then

  gy2 gy1 - 0 <> if
    gx2 gx1 - 8 LSHIFT gy2 gy1 - /
    gc2 gc1 - 8 LSHIFT gy2 gy1 - /
  else
    0 0
  then
  to delta_c1
  to deltax_l1

  gx1 8 LSHIFT to startx
  gc1 8 LSHIFT to startc
  gy2 1+ gy1 do                 \ Calculate segment [Y1|Y2]
    startx 8 RSHIFT LeftX[] i pel-ndx !
    deltax_l1 +to startx
    startc 8 RSHIFT LColor[] i pel-ndx !
    delta_c1 +to startc
  loop

  gy3 gy2 - 0 <> if
    gx3 gx2 - 8 LSHIFT gy3 gy2 - /
    gc3 gc2 - 8 LSHIFT gy3 gy2 - /
  else
    0 0
  then
  to delta_c2
  to deltax_l2

  gx2 8 LSHIFT to startx
  gc2 8 LSHIFT to startc

  gy3 1+ gy2 do                 \ Calculate segment [Y2|Y3]
    startx 8 RSHIFT LeftX[] i pel-ndx !
    deltax_l2 +to startx
    startc 8 RSHIFT LColor[] i pel-ndx !
    delta_c2 +to startc
  loop

  gy3 gy1 - 0 <> if
    gx3 gx1 - 8 LSHIFT gy3 gy1 - /
    gc3 gc1 - 8 LSHIFT gy3 gy1 - /
  else
    0 0
  then
  to delta_c3
  to deltax_r

  gx1 8 LSHIFT to startx
  gc1 8 LSHIFT to startc

  gy3 1+ gy1 do                 \ Calculate segment [Y1|Y3]
    i gy2 = if
      startx 8 RSHIFT to qx
      startc 8 RSHIFT to qc
    then
    startx 8 RSHIFT RightX[] i pel-ndx !
    deltax_r +to startx
    startc 8 RSHIFT RColor[] i pel-ndx !
    delta_c3 +to startc
  loop

  gx2 qx - 0 <> if
    gc2 qc - 8 LSHIFT gx2 qx - / %Delta !
    gy3 1+ gy1 do
      gbuf
      LeftX[] i pel-ndx @
      RightX[] i pel-ndx @
      i
      LColor[] i pel-ndx @
      RColor[] i pel-ndx @
      GHLine
    loop
  then
;

decimal

\ ---[ Random Polygons Demo ]----------------------------------------

65536 HeapAllot VPage0[]

: SetColor ( c r g b -- )
  swap                  \ c r b g
  rot                   \ c b g r
  3 pick $03C8 CPortOut
  $03C9 CPortOut
  $03C9 CPortOut
  $03C9 CPortOut
  drop
;

: GrayScale
  63 0 do
    i i i i SetColor
  loop
;

: GFillPoly
  $13 InitGraph
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
    63 rnd              \ &buf x1 y1 x2 y2 x3 y3 c1
    63 rnd              \ &buf x1 y1 x2 y2 x3 y3 c1 c2
    63 rnd              \ &buf x1 y1 x2 y2 x3 y3 c1 c2 c3
    gpoly3              \ --
    WaitRetrace
    VPage0[] BlitBuffer
  repeat
  key drop
  CloseGraph
;

: GFillPoly1
  $13 InitGraph
  GrayScale
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
    63 rnd              \ &buf x1 y1 x2 y2 x3 y3 c1
    63 rnd              \ &buf x1 y1 x2 y2 x3 y3 c1 c2
    63 rnd              \ &buf x1 y1 x2 y2 x3 y3 c1 c2 c3
    gpoly3              \ --
    WaitRetrace
    VPage0[] BlitBuffer
  repeat
  key drop
  CloseGraph
;

\ ---[ Rotating Cube Demo ]------------------------------------------

S{
  {1WORD}-DEF :: .cX
  {1WORD}-DEF :: .cY
  {1WORD}-DEF :: .cZ
}S cube-obj

S{
  {1WORD}-DEF :: .obj#
  {1WORD}-DEF :: .zavg
}S zsum-obj

{1FLOAT}-DEF 360 [] sc[]-obj sc-ndx

{1FLOAT}-DEF 360 * HeapAllot Sine[]
{1FLOAT}-DEF 360 * HeapAllot Cosine[]

zsum-obj 6 [] zsum[]-obj zsum-ndx

zsum[]-obj HeapAllot ZSum[]     \ for zbuffering
zsum-obj HeapAllot TempZSum[]   \ for swapping

value xangle
value yangle
value zangle
value distance
value direction

8 constant #OfObjects \ in a cube, this is the # of corners

cube-obj #OfObjects [] cube[]-OBJ cube-NDX

cube[]-obj HeapAllot cube[]

cube[]-obj HeapAllot temp[]

floating

: CreateLookupTables() ( -- )
  360 0 do
    i S>F 3.14159265 F* 180.0 F/ FSINCOS
    Cosine[] i sc-ndx F!
    Sine[]   i sc-ndx F!
  loop
;

integer

\ ---[ Create Vector Object ]----------------------------------------

: SetObject ( x y z obj# -- )
  cube[] swap cube-ndx >R R@ .cZ ! R@ .cY ! R> .cX !
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

floating

: RotateVectors()
  #OfObjects 0 do
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

integer

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

: ZBuffering() ( -- )
  ZAverage
  #OfObjects 1- 0 do
    #OfObjects i 1+ do
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

value ds.buf
value ds.a
value ds.b
value ds.c
value ds.d

value ca
value cb
value cc

: DrawSide ( &buf a b c d -- )
  to ds.d to ds.c to ds.b to ds.a
  to ds.buf

  ds.buf
  temp[] ds.a cube-ndx dup .cX @ swap .cY @
  temp[] ds.b cube-ndx dup .cX @ swap .cY @
  temp[] ds.c cube-ndx dup .cX @ swap .cY @
  ca cb cc
  gpoly3
  ds.buf
  temp[] ds.c cube-ndx dup .cX @ swap .cY @
  temp[] ds.d cube-ndx dup .cX @ swap .cY @
  temp[] ds.a cube-ndx dup .cX @ swap .cY @
  cc cb ca
  gpoly3
;

: DrawCube
  RotateVectors()
  ZBuffering()
  VPage0[] 65536 0 fill
  6 3 do
    VPage0[]
    ZSum[] i zsum-ndx .obj# @
    case
      0 of 0 1 2 3 DrawSide endof
      1 of 4 5 6 7 DrawSide endof
      2 of 0 1 5 4 DrawSide endof
      3 of 3 2 6 7 DrawSide endof
      4 of 0 4 7 3 DrawSide endof
      5 of 1 5 6 2 DrawSide endof
    endcase
  loop
;

value %counter

: SetColor ( c r g b -- )
  swap                  \ c r b g
  rot                   \ c b g r
  3 pick $03C8 CPortOut
  $03C9 CPortOut
  $03C9 CPortOut
  $03C9 CPortOut
  drop
;

: InitData
   25 to xangle                 \ init variables and arrays
  125 to yangle
  275 to zangle
  200 to distance
    1 to direction
    0 to %counter

  VPage0[] 64000 0 fill
  CreateLookupTables()          \ init the sine/cosine tables
  InitCube                      \ init the cube[] structure
  $13 InitGraph
;

: InitPalette
  63 0 do                       \ A gray-scale palette
    i i i i SetColor
  loop
;

: RandomSelects
  63 rnd to ca
  63 rnd to cb
  63 rnd to cc
;

: KeyCheck
  key? if
    key 32 = if
      RandomSelects             \ change colors if space bar hit
      0 to %counter
      1
    else
      0
    then
  else
    1
  then
;

: UpdateAngles
  3 +to xangle  xangle 359 > if 0 to xangle then
  3 +to yangle  yangle 359 > if 0 to yangle then
  -2 +to zangle zangle 1 < if 359 to zangle then
  1 +to %counter
;

: UpdateDirection
  direction 1 = if              \ update distance & direction
    1 +to distance
    distance 599 > if 0 to direction 600 to distance then
  else
    -1 +to distance
    distance 200 < if 1 to direction 200 to distance then
  then
;

: UpdateCounter
  %counter 200 > if
    0 to %counter
    RandomSelects               \ change colors after 500 loops
  then
;

: GCube ( -- )
  InitData
  InitPalette
  RandomSelects                 \ start with random palette selection
  begin                         \ repeat until a key is pressed
    KeyCheck
  while
    UpdateAngles                \ adjust the rotation angles
    UpdateCounter               \ adjust color change counter
    DrawCube                    \ draw cube in new position
    WaitRetrace                 \ delay
    VPage0[] BlitBuffer         \ blast to vga memory
  repeat
  CloseGraph
;

: GCube1 ( -- )
  InitData
  InitPalette
  RandomSelects
  begin                         \ repeat until a key is pressed
    KeyCheck
  while
    UpdateDirection             \ adjust Z direction movements
    UpdateAngles                \ adjust the rotation angles
    UpdateCounter               \ adjust color change counter
    DrawCube                    \ draw cube in new position
    WaitRetrace                 \ delay
    VPage0[] BlitBuffer         \ blast to vga memory
  repeat
  CloseGraph
;

cr .( GotTK3: GFillPoly GFillPoly1 GCube GCube1 ) cr

