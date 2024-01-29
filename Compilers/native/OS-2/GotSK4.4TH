\ ---[ Code Addendum 04 ]-----------------------------[12/12/2006]---
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
\ Save as GotSK4.4th in your \os2forth\forth directory

exists [GotSK4] [if]
  forget [GotSK4]
[then]

: [GotSK4] ;

\ This will attempt to include the GotSK2.4th and GotSK3.4th files.

include gotsk3.4th

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

S{
  {1WORD}-DEF :: .bheight
  {1WORD}-DEF :: .bwidth
  24 31 *     :: .bimage
}S sprite-obj

S{
  {1WORD}-DEF :: .vbX
  {1WORD}-DEF :: .vbY
  {1WORD}-DEF :: .vbZ
  {1WORD}-DEF :: .vbC
}S vball-obj

\ ---[ Variables ]---------------------------------------------------

\ Defines a Floating Point array with 360 elements
{1FLOAT}-DEF 360 [] sc[]-obj sc-ndx
\ Allocates a pair of arrays in the dictionary
create Sine[]   here sc[]-obj dup allot 0 fill
create Cosine[] here sc[]-obj dup allot 0 fill

variable xangle
variable yangle
variable zangle
variable distance
variable direction

vball-obj 8 [] object[]-OBJ object-NDX
create object[] here object[]-OBJ dup allot 0 fill
create object2[] here object[]-obj dup allot 0 fill

create temp[] here object[]-OBJ dup allot 0 fill

\ single dummy vector ball used for swapping

create dummy here vball-obj dup allot 0 fill

\ A display buffer for this demo

create VPage5[] here 65536 dup allot 0 fill

create BollPalette1[]
\ 27*3 bytes
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
         0 C,  0 C, 47 C, \ BLUE
         0 C, 47 C,  0 C, \ GREEN
        47 C,  0 C,  0 C, \ RED

\ Need an additional 3 colors, so I add a second version of both the
\ palette and palette setup

: SetBollPalette1
  0 $03C8 CPortOut
  27 3 * 0 do
    BollPalette1[] i + C@ $03C9 CPortOut
  loop
;

\ ---[ Sine Tables ]-------------------------------------------------

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
  object[] swap object-ndx >R R@ .vbZ ! R@ .vbY ! R> .vbX !
;

: CreateVectorObject() ( -- )
\ the arrangement of the bolls will be in a cube pattern
  -30 -30  30 0 SetObject               \ back top left coords
   30 -30  31 1 SetObject               \ back top right coords
  -30  30  32 2 SetObject               \ back bottom left coords
   30  30  33 3 SetObject               \ back bottom right coords
  -30 -30 -30 4 SetObject               \ front top left coords
   30 -30 -31 5 SetObject               \ front top right coords
  -30  30 -32 6 SetObject               \ front bottom left coords
   30  30 -33 7 SetObject               \ front bottom right coords
   object[] object2[] object[]-obj cmove
;

\ ---[ DrawObject ]--------------------------------------------------

value %ofs
value %color
value %src
value %dst

: DrawBollSprite ( &obj x y -- )
  6 LSHIFT dup 2 LSHIFT + + to %ofs
  to %src
  VPage5[] to %dst
  %src .bwidth @
  %src .bheight @ 0 do           \ .w
    dup 0 do                    \ .w
      dup                       \ .w .w
      j *                       \ .w .w*j
      i +                       \ .w .w*j+i
      %src .bimage + C@         \ .w src[.w*j+i]
      dup                       \ .w src[.w*j+i] src[.w*j+i]
      if                        \ .w src[.w*j+i]
        %dst %ofs + i + C!      \ .w
      else                      \ .w src[.w*j+i]
        drop                    \ .w                 \ skip if 0
      then                      \ .w
    loop                        \ .w
    320 +to %ofs                \ .w
  loop                          \ .w
  drop                          \ --
;

\ ---[ DrawVectorObject ]--------------------------------------------
\ This is the complex major routine in the demo

value dvo:i
value dvo:j
value dvo:k
value dvo:a
value dvo:b
value dvo:nx
value dvo:ny
value dvo:nz
variable dvo:xofs

floating

: RotateVectors() ( &obj -- )
  dup object[] = if 240 else 80 then dvo:xofs !
\ first we have to do all the rotations
  8 0 do
    dup i object-ndx >R
  \ rotate around the x-axis
  \ ny=(int)(object[i].y*cosine[xangle]-object[i].z*sine[xangle]);
    R@ .vbY @ S>F Cosine[] xangle @ sc-ndx F@ F*
    R@ .vbZ @ S>F Sine[]   xangle @ sc-ndx F@ F* F- F>S to dvo:ny

  \ nz=(int)(object[i].y*sine[xangle]+object[i].z*cosine[xangle]);
    R@ .vbY @ S>F Sine[]   xangle @ sc-ndx F@ F*
    R@ .vbZ @ S>F Cosine[] xangle @ sc-ndx F@ F* F+ F>S to dvo:nz

  \ nx=(int)(object[i].x);
    R> .vbX @ to dvo:nx

    temp[] i object-ndx >R
    dvo:nx R@ .vbX !
    dvo:ny R@ .vbY !
    dvo:nz R@ .vbZ !

  \ rotate around the y-axis
  \ nx=(int)(temp[i].x * cosine[yangle] +temp[i].z * sine[yangle]);
    R@ .vbX @ S>F Cosine[] yangle @ sc-ndx F@ F*
    R@ .vbZ @ S>F Sine[]   yangle @ sc-ndx F@ F* F+ F>S to dvo:nx

  \ nz=(int)(-temp[i].x * sine[yangle]+temp[i].z * cosine[yangle]);
    R@ .vbX @ negate S>F Sine[]   yangle @ sc-ndx F@ F*
    R@ .vbZ @        S>F Cosine[] yangle @ sc-ndx F@ F* F+ F>S to dvo:nz

    dvo:nx R@ .vbX !
    dvo:nz R@ .vbZ !

  \ rotate around the z-axis
  \ nx=(int)(temp[i].x * cosine[zangle]-temp[i].y * sine[zangle]);
    R@ .vbX @ S>F Cosine[] zangle @ sc-ndx F@ F*
    R@ .vbY @ S>F Sine[]   zangle @ sc-ndx F@ F* F- F>S to dvo:nx

  \ ny=(int)(temp[i].x * sine[zangle]+temp[i].y * cosine[zangle]);
    R@ .vbX @ S>F Sine[]   zangle @ sc-ndx F@ F*
    R@ .vbY @ S>F Cosine[] zangle @ sc-ndx F@ F* F+ F>S to dvo:ny

    dvo:nx R@ .vbX !
    dvo:ny R@ .vbY !

  \ now we have to push the z coordinates into the view area
  \ temp[i].z-=distance;
    R@ .vbZ @ distance @ - R@ .vbZ !

  \ finally we project & copy the new x,y,z to a temporary array
  \ temp[i].x=(int)((temp[i].x*256)/temp[i].z+160);
    R@ .vbX @ 256 * R@ .vbZ @ / dvo:xofs @ + R@ .vbX !

  \ temp[i].y=(int)((temp[i].y*256)/temp[i].z+120);
    R@ .vbY @ 256 * R@ .vbZ @ / 100 + R@ .vbY !

  \ temp[i].color=object[i].color;
    R>
    object[] i object-ndx .vbC @ swap .vbC !
  loop
  drop
;

integer

\ ---[ ZBuffering ]--------------------------------------------------
\ Sorts the image structures based on the .vbZ value of each.
\ the z with most positive value should be in lowest index
\ I used bubble sort here because there are only 8 balls :-)
\ -------------------------------------------------------------------

: ZBuffering() ( -- )
  7 0 do
    8 i 1+ do
      temp[] j object-ndx .vbZ @        \ a=temp[j].z;
      temp[] i object-ndx .vbZ @        \ b=temp[i].z;
      >= if                             \ if (a >= b) {
      \ swap the two vector balls - this moves the struct
        temp[] j object-ndx dummy vball-obj cmove
        temp[] i object-ndx temp[] j object-ndx vball-obj cmove
        dummy temp[] i object-ndx vball-obj cmove
      then
    loop
  loop
;

: DrawVectorObjects()
  object[] RotateVectors()
  ZBuffering()

\ we're done everything, so draw the objects!
\ first we erase the old stuff off the drawing zone
  VPage0[] ZeroIt
\ and then draw the new ball positions
  8 0 do
    Boll[]
    temp[] i object-ndx >R
    R@ .vbX @ 11 -
    R> .vbY @ 15 -
    DrawBollSprite
  loop
\ do second object batch
  object2[] RotateVectors()
  ZBuffering()
  8 0 do
    Boll[]
    temp[] i object-ndx >R
    R@ .vbX @ 11 -
    R> .vbY @ 15 -
    DrawBollSprite
  loop
;

: bolls
  1 to %UseVR
  0 to %Speed
  360 rnd xangle !              \ init variables and arrays
  360 rnd yangle !
  360 rnd zangle !
  256 distance !
    1 direction !

  CreateLookupTables()          \ init the sine/cosine tables
  CreateVectorObject()          \ init the object[] structure

  24 %B !
  25 %G !
  26 %R !
  VPage0[] ZeroIt
  VPage5[] ZeroIt
  Background

  $13 InitGraph
  SetBollPalette1
  begin                         \ repeat until a key is pressed
    KeyCheck
  while
    direction @ 1 = if          \ update distance & direction
      1 distance +!
      distance @ 599 > if 0 direction ! 600 distance ! then
    else
      -1 distance +!
      distance @ 255 < if 1 direction ! 256 distance ! then
    then
  \ update the angles
    3 xangle +! xangle @ 359 > if   0 xangle ! then
    3 yangle +! yangle @ 359 > if   0 yangle ! then
   -2 zangle +! zangle @ 1   < if 359 zangle ! then

    DrawVectorObjects()         \ Draw to the video buffer

    %Speed if OneFrame then
    CombineVPages
    VPage5[] VPage0[] AddVPage
    VPage0[] BlitBuffer
    %UseVR if WaitRetrace then
    VPage5[] Zeroit
    VPage3[] ZeroIt               \ erase the buffers for
    VPage2[] ZeroIt               \ the next cycle
    VPage1[] ZeroIt
  repeat
  CloseGraph
;

cr .( Enter 'bolls' to see the demo) cr

