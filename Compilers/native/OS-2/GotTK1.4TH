\ ---[ Code Addendum 01 ]-----------------------------[01/05/2007]---
\
\          For the Graphics of the Third Kind Part I column
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
\ Save as GotTK1.4th in your \os2forth\forth directory

\ This Code Addendum is stand-alone.  Modules required from previous
\ Addendums are included.

exists [GotTK1] [if]
  forget [GotTK1]
[then]

: [GotTK1] ;

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

\ ---[ Bresenham Line Algorithm Revisited ]--------------------------

\ ---[ (putpixel) ]--------------------------------------------------
\ Required for the Line routine.

\ on entry, DI=&buf[0]
\           ax=x
\           bx=y
\           dx=c

label (putpixel) ( -- )
                    es  push
                 ax di  add     \ di=&buf[x]
                6 # cl  mov
                 bx cl  shl     \ bx*64
                 bx di  add     \ di=&buf[y*64+x]
                2 # cl  mov
                 bx cl  shl     \ bx=y*256
                 bx di  add     \ di=&buf[y*320+x]
                 dl al  mov
         es: al 0 [di]  mov
                    es  pop
                        ret
previous previous also

\ --------------------
\ Stack frame for Line
\ --------------------
\ &buf  5 cells [bp]
\ x1    4 cells [bp]
\ y1    3 cells [bp]
\ x2    2 cells [bp]
\ y2    1 cells [bp]
\ c     0 cells [bp]
\ sx   -1 cells [bp]
\ sy   -2 cells [bp]
\ dx1  -3 cells [bp]
\ dy1  -4 cells [bp]
\ dx2  -5 cells [bp]
\ dy2  -6 cells [bp]
\ x    -7 cells [bp]
\ y    -8 cells [bp]
\ m    -9 cells [bp]
\ n   -10 cells [bp]
\ k   -11 cells [bp]
\ cnt -12 cells [bp]
\ di

code Line ( &buf x1 y1 x2 y2 c -- )
                    bx  push    \ all parms to stack
                 bp sp  xchg    \ setup stack frame
         13 cells # sp  sub     \ create local variables
                    di  push
                 ax ax  xor
     ax -11 cells [bp]  mov     \ k=0 initially
      ax -6 cells [bp]  mov     \ dy2=0 also
\ sy=y2-y1
       1 cells [bp] ax  mov     \ ax=y2
       3 cells [bp] ax  sub     \ ax=y2-y1
      ax -2 cells [bp]  mov     \ sy=y2-y1
\ sx=x2-x1
       2 cells [bp] ax  mov     \ ax=x2
       4 cells [bp] ax  sub     \ ax=x2-x1
      ax -1 cells [bp]  mov     \ sx=x2-x1
\ check x slope
                 bx bx  xor     \ dx1=0 initially
                0 # ax  cmp
                  1 L#  jle
                    bx  inc     \ if (sx>0) dx1=+1
                  2 L#  ju
1 L:              2 L#  je
                    bx  dec     \ else if (sx<0) dx1=-1
2 L:                            \ else dx1=0
      bx -3 cells [bp]  mov     \ store dx1 with -1|0|+1
      bx -5 cells [bp]  mov     \ store dx1 to dx2 also

\ since ax=sx right now, let's also set m=abs(sx)
                 ax ax  or
                  3 L#  jns     \ already positive?
                    ax  neg     \ nope - change it
3 L:
      ax -9 cells [bp]  mov
\ check y slope
      -2 cells [bp] ax  mov     \ ax=sy
                 bx bx  xor     \ dy1=0 initially
                0 # ax  cmp
                  4 L#  jle
                    bx  inc     \ if (sy>0) dy1=+1
                  5 L#  ju
4 L:              5 L#  je
                    bx  dec     \ else if (sy<0) dy1=-1
5 L:                            \ else dy1=0
      bx -4 cells [bp]  mov     \ store dy1 with -1|0|+1
\ and now set n=abs(sy)
                 ax ax  or
                  6 L#  jns     \ already positive?
                    ax  neg     \ nope - change it
6 L:
     ax -10 cells [bp]  mov     \ n=abs(sy)
\ -------------------------------------------------------------------
\             At this point, m=abs(sx) and n=abs(sy)
\ -------------------------------------------------------------------
      -9 cells [bp] ax  cmp
                  7 L#  jl      \ if n<m continue, else
      -9 cells [bp] bx  mov     \ if (m<n) m=abs(sy) and n=abs(sx)
      ax -9 cells [bp]  mov     \ so we just swap m and n
     bx -10 cells [bp]  mov
                 bx bx  xor     \ bx=0
      bx -5 cells [bp]  mov     \ dx2=0
      -4 cells [bp] bx  mov     \ bx=dy1
      bx -6 cells [bp]  mov     \ dy2=dy1
7 L:
       4 cells [bp] ax  mov
      ax -7 cells [bp]  mov     \ x=x1
       3 cells [bp] ax  mov
      ax -8 cells [bp]  mov     \ y=y1

      -9 cells [bp] ax  mov
                    ax  inc
     ax -12 cells [bp]  mov     \ cnt=m+1
8 L:
       5 cells [bp] di  mov     \ di=&buf[0]
      -7 cells [bp] ax  mov     \ ax=x
      -8 cells [bp] bx  mov     \ bx=y
       0 cells [bp] dx  mov     \ dx=c
         (putpixel) #)  call    \ plot the pixel
     -10 cells [bp] ax  mov     \ ax=n
     ax -11 cells [bp]  add     \ k+=n
     -11 cells [bp] ax  mov     \ ax=k
      -9 cells [bp] ax  cmp
                  9 L#  jg      \ k<m?
      -5 cells [bp] ax  mov     \ ax=dx2
      ax -7 cells [bp]  add     \ x+=dx2
      -6 cells [bp] ax  mov     \ ax=dy2
      ax -8 cells [bp]  add     \ y+=dy2
                 10 L#  ju
9 L:
      -9 cells [bp] ax  mov     \ ax=m
     ax -11 cells [bp]  sub     \ k -= m
      -3 cells [bp] ax  mov     \ ax=dx1
      ax -7 cells [bp]  add     \ x+=dx1
      -4 cells [bp] ax  mov     \ ax=dy1
      ax -8 cells [bp]  add     \ y+=dy2
10 L:
        -12 cells [bp]  dec     \ decrement the counter
                  8 L#  jnz     \ loop until 0

                    di  pop     \ restore register
         13 cells # sp  add     \ lose locals
                 bp sp  xchg    \ restore stack frame
          6 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand
\ --------------------------------------------[End Bresenham Line]---


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
}S seg-obj

cube-obj 8 [] cube[]-obj cube-ndx
seg-obj 12 [] cseg[]-obj cseg-ndx

\ ---[ Array Allocations ]-------------------------------------------

cube[]-obj HeapAllot Cube[]
cube[]-obj HeapAllot Temp[]
65536 HeapAllot VPage0[]        \ allocate double buffer

\ Specify the line segments that make up the cube

create CSegs[]
0 , 1 ,                 \ a to b
1 , 2 ,                 \ b to c
2 , 3 ,                 \ c to d
3 , 0 ,                 \ d to a
4 , 5 ,                 \ e to f
5 , 6 ,                 \ f to g
6 , 7 ,                 \ g to h
7 , 4 ,                 \ h to e
0 , 4 ,                 \ a to e
1 , 5 ,                 \ b to f
2 , 6 ,                 \ c to g
3 , 7 ,                 \ d to h

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

: DrawCube ( -- )
  RotateVectors
  12 0 do
    VPage0[]                    \ draw image to buffer
    CSegs[] i cseg-ndx >R
    temp[] R@ .pa @ cube-ndx dup .cX @ swap .cY @
    temp[] R> .pb @ cube-ndx dup .cX @ swap .cY @
    1
    Line
  loop
  WaitRetrace                   \ delay
  VPage0[] BlitBuffer           \ display buffer data
  12 0 do
    VPage0[]                    \ erase image from buffer
    CSegs[] i cseg-ndx >R
    temp[] R@ .pa @ cube-ndx dup .cX @ swap .cY @
    temp[] R> .pb @ cube-ndx dup .cX @ swap .cY @
    0
    Line
  loop
;

: cube ( -- )
   25 to xangle           \ init variables and arrays
  125 to yangle
  275 to zangle
  200 to distance

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

: cube1 ( -- )
   25 to xangle           \ init variables and arrays
  125 to yangle
  275 to zangle
  200 to distance
    1 to direction

  VPage0[] 64000 0 fill
  CreateLookupTables      \ init the sine/cosine tables
  InitCube                \ init the cube[] structure
  $13 InitGraph
  begin                   \ repeat until a key is pressed
    key? not
  while
    direction if        \ update distance & direction
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
    DrawCube
  repeat
  key drop
  CloseGraph
;

cr .( GotTK1: cube cube1) cr

