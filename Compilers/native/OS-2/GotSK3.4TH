\ ---[ Code Addendum 03 ]-----------------------------[12/14/2006]---
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
\ Save as GotSK3.4th in your \os2forth\forth directory

exists [GotSK3] [if]
  forget [GotSK3]
[then]

: [GotSK3] ;

include gotsk2.4th
decimal

create SquareB[]
base @ binary
00000000 C, 00000000 C,
01111111 C, 11111110 C,
01111111 C, 11111110 C,
01111111 C, 11111110 C,
01110000 C, 00001110 C,
01110000 C, 00001110 C,
01110000 C, 00001110 C,
01110000 C, 00001110 C,
01110000 C, 00001110 C,
01110000 C, 00001110 C,
01110000 C, 00001110 C,
01110000 C, 00001110 C,
01111111 C, 11111110 C,
01111111 C, 11111110 C,
01111111 C, 11111110 C,
00000000 C, 00000000 C,
base !

variable %xmove
variable %dir

: DrawPackedSprite1 ( &image x y c -- )
  >R rot to %src R>             \ x y c
  16 0 do                       \ x y c
    %src C@ 1 +to %src          \ x y c chi
    8 LSHIFT                    \ x y c chi<<8
    %src C@ 1 +to %src          \ x y c chi<<8 clo
    OR                          \ x y c n
    16 0 do                     \ x y c n
      3 pick i +                \ x y c n x+i
      3 pick j +                \ x y c n x+i y+j
      2 pick $8000 and if       \ x y c n
        3 pick                  \ x y c n x+i y+j c
      else
        0
      then
      %Speed if
        FastPlot
      else
        Plot
      then
      1 LSHIFT
    loop
    drop
  loop
  2drop
  drop
;

: MoveSprite
  $13 InitGraph
  0 to %Speed
  1 to %UseVR
  0 %xmove !
  1 %dir !
  begin
    KeyCheck
  while
    SquareB[] %xmove @  0 1 DrawPackedSprite1
    SquareB[] %xmove @ 30 2 DrawPackedSprite1
    SquareB[] %xmove @ 60 3 DrawPackedSprite1
    SquareB[] %xmove @ 90 4 DrawPackedSprite1
    %dir @ %xmove +!
    %xmove @ 319 16 - = if -1 %dir ! then
    %xmove @ 0 = if 1 %dir ! then
    %UseVR if WaitRetrace then
  repeat
  CloseGraph
;

code FastBPlot ( &buf x y c -- )
                                \ c in bx on entry as TOS
                    bx  push    \ put all parameters onto the stack
                 bp sp  xchg    \ setup the stack frame
                    es  push    \ save registers to be used
                    di  push
                    ds  push
                    es  pop
       \ Calculate Y*320+X
       2 cells [bp] di  mov     \ di=x
       1 cells [bp] ax  mov     \ ax=y
                6 # cl  mov
                 ax cl  shl     \ ax=y*64
                 ax di  add     \ di=y*64+x
                2 # cl  mov
                 ax cl  shl     \ ax=y*256
                 ax di  add     \ di=y*320+x
       3 cells [bp] di  add     \ di=&buf[y*320+x]
       0 cells [bp] ax  mov     \ al=c
         es: al 0 [di]  mov     \ plot the pixel
                    di  pop     \ restore registers
                    es  pop
                 bp sp  xchg    \ restore stack frame
          4 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

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

\ ZeroIt uses 32-bit stores to clear the specified buffer

code ZeroIt ( &dst -- )
                    es  push
                    di  push
                    ds  push
                    es  pop
                 bx di  mov
            16000 # cx  mov
                 ax ax  xor
                        cld
                        rep
                    ax  stos
                    di  pop
                    es  pop
                    bx  pop
                    end-code
                    no-expand

create VPage0[] here 65536 dup allot 0 fill \ main display
create VPage1[] here 65536 dup allot 0 fill \ blue palette
create VPage2[] here 65536 dup allot 0 fill \ green palette
create VPage3[] here 65536 dup allot 0 fill \ red palette
create VPage4[] here 65536 dup allot 0 fill \ background

value %src

: DrawPackedSpriteToBuffer ( &buf &image x y c -- )
  >R rot to %src R>             \ &buf x y c
  16 0 do                       \ &buf x y c
    %src C@ 1 +to %src          \ &buf x y c chi
    8 LSHIFT                    \ &buf x y c chi<<8
    %src C@ 1 +to %src          \ &buf x y c chi<<8 clo
    OR                          \ &buf x y c n
    16 0 do                     \ &buf x y c n
      dup $8000 and if          \ &buf x y c n
        4 pick                  \ &buf x y c n &buf
        4 pick i +              \ &buf x y c n &buf x+i
        4 pick j +              \ &buf x y c n &buf x+i y+j
        4 pick                  \ &buf x y c n &buf x+i y+j c
        FastBPlot               \ &buf x y c n
      then                      \ &buf x y c n
      1 LSHIFT                  \ &buf x y c n<<1
    loop                        \ &buf x y c n<<1
    drop                        \ &buf x y c
  loop                          \ &buf x y c
  2drop                         \ &buf x
  2drop                         \ --
;

\ AddVPage combines &src to &dst - but only the non-zero pixels
\ The pixels in the &src buffer take precedence over those in the
\ %dst buffer, so any pixel in %src will overwrite those in %dst.
\ This allows us to simulate a planar mode where we are drawing
\ different data to the specific planes, and have configured the
\ display so the planes have an order of precedence.

code AddVPage ( &src &dst -- )
                    bx  push
                 bp sp  xchg
                    es  push
                    di  push
                    si  push

                    ds  push
                    es  pop
                        cld
       0 cells [bp] di  mov
       1 cells [bp] si  mov
            64000 # cx  mov
1 L:
             0 [si] al  mov
                0 # al  cmp
                  2 L#  je
         es: al 0 [di]  mov
2 L:
                    di  inc     \ increment plotted or not
                    si  inc
                    cx  dec
                  1 L#  jnz

                    si  pop     \ restore registers
                    di  pop
                    es  pop
                 bp sp  xchg    \ restore stack frame
          2 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

: CombineVPages ( -- )
  VPage4[] VPage0[] 64000 cmove
  VPage3[] VPage0[] AddVPage
  VPage2[] VPage0[] AddVPage
  VPage1[] VPage0[] AddVPage
;

: VBLine ( &buf x y1 y2 c -- )
  swap                          \ &buf x y1 c y2
  2 pick - 1+                   \ &buf x y1 c y2-y1
  0 do                          \ &buf x y1 c
    3 pick                      \ &buf x y1 c &buf
    3 pick                      \ &buf x y1 c &buf x
    3 pick                      \ &buf x y1 c &buf x y1
    i +                         \ &buf x y1 c &buf x y1+i
    3 pick                      \ &buf x y1 c &buf x y1+i c
    FastBPlot                   \ &buf x y1 c
  loop                          \ &buf x y1 c
  2drop                         \ &buf x
  2drop                         \ --
;

: HBLine ( &buf x1 x2 y c -- )
  rot                           \ &buf x1 y c x2
  3 pick -                      \ &buf x1 y c len
  swap >R >R                    \ &buf x1 y
  320 * +                       \ &buf ofs
  +                             \ &buf[ofs]
  R> R>                         \ &buf[ofs] len c
  fill                          \ --
;

\ Draws the background grid image to VPage4[]

: Background
  VPage4[] ZeroIt
  42 0 do
    VPage4[] i 8 * 0 199 7 VBLine
  loop
  VPage4[] 319 0 199 7 VBLine
  21 0 do
    VPage4[] 0 319 i 10 * 7 HBLine
  loop
  VPage4[] 0 319 199 7 HBLine
;

\ Simulate structure usage:

: .delay     0 + ;
: .basedelay 1 cells + ;
: .&image    2 cells + ;
: .xcoord    3 cells + ;
: .xinc      4 cells + ;
: .xmin      5 cells + ;
: .xmax      6 cells + ;
: .ycoord    7 cells + ;
: .yinc      8 cells + ;
: .ymin      9 cells + ;
: .ymax     10 cells + ;
: .color    11 cells + ;

\ These allow a level in indirection that I will utilize in the next
\ column, when I add the Bolls animations in.

variable %B  24 % !
variable %G  25 % !
variable %R  26 % !

create Objects[]
1 , 21 , Diamond[] ,  44 , 2 ,  40 , 256 ,   8 , 0 , 0 , 195 , %R ,
1 , 15 , Ovoid[]   , 148 , 2 ,  56 , 240 ,  72 , 0 , 0 , 195 , %R ,
1 , 23 , Diamond[] ,  44 , 2 ,  40 , 256 , 128 , 0 , 0 , 195 , %R ,
1 , 13 , Ovoid[]   ,  60 , 0 ,   0 , 320 ,  72 , 2 , 0 , 160 , %B ,
1 , 11 , Diamond[] , 104 , 0 ,   0 , 320 ,  72 , 2 , 0 , 160 , %B ,
1 ,  8 , Ovoid[]   , 148 , 0 ,   0 , 320 ,  72 , 2 , 0 , 164 , %B ,
1 ,  9 , Diamond[] , 192 , 0 ,   0 , 320 ,  72 , 2 , 0 , 164 , %B ,
1 , 14 , Ovoid[]   , 236 , 0 ,   0 , 320 ,  72 , 2 , 0 , 160 , %B ,
1 ,  8 , Diamond[] , 100 , 4 ,   4 , 300 ,  24 , 3 , 0 , 180 , %G ,
1 ,  9 , Ovoid[]   , 124 , 4 ,   4 , 300 ,  48 , 3 , 0 , 180 , %G ,
1 ,  7 , Diamond[] , 148 , 4 ,   4 , 300 ,  72 , 3 , 0 , 180 , %G ,
1 ,  6 , Ovoid[]   , 172 , 4 ,   4 , 300 ,  96 , 3 , 0 , 180 , %G ,
1 ,  5 , Diamond[] , 196 , 4 ,   4 , 300 , 120 , 3 , 0 , 180 , %G ,

: DrawObject ( &obj -- )
  >R
  R@ .color @                   \ &c     (variable address, not data)
  case                          \
    %B of VPage1[] endof        \ &VPage1[]
    %G of VPage2[] endof        \ &VPage2[]
    %R of VPage3[] endof        \ &VPage3[]
  endcase                       \ &buf
  R@ .&image @                  \ &buf &image
  R@ .xcoord @                  \ &buf &image x
  R@ .ycoord @                  \ &buf &image x y
  R> .color @ @                 \ &buf &image x y c
  DrawPackedSpriteToBuffer      \ --
;

value %currobj

: OneFrame
  13 0 do                         \ cycle thru the objects
    Objects[] i 12 cells * + >R   \ put base obj adr on return stack
    R@ .delay @ 1- dup R@ .delay ! \ update the delay counter
    if
      R@ .basedelay @ R@ .delay ! \ reset to .basedelay
      R@ .xcoord @ dup            \ inc x; reverse at xmin/xmax
      R@ .xmin @ < over
      R@ .xmax @ > or if
        R@ .xinc @ negate R@ .xinc !
      then
      R@ .xinc @ + R@ .xcoord !   \ add XInc to current XCoord
      R@ .ycoord @ dup            \ inc y; reverse at ymin/ymax
      R@ .ymin @ < over
      R@ .ymax @ > or if
        R@ .yinc @ negate R@ .yinc !
      then
      R@ .yinc @ + R@ .ycoord !   \ add YInc to current YCoord
      R> DrawObject               \ draw new image location
    else
      R> drop                     \ lose obj pointer
    then                          \ and check next obj
  loop
;

: AnimatePacked
  begin
    key? not
  while
    OneFrame
    CombineVPages                 \ all for one
    WaitRetrace                   \ pause
    VPage0[] BlitBuffer           \ and blit to the screen
    VPage3[] ZeroIt               \ erase the buffers for
    VPage2[] ZeroIt               \ the next cycle
    VPage1[] ZeroIt
  repeat
  key drop
;

: SpriteDemo
  $13 InitGraph
  1 %B !
  2 %G !
  4 %R !
  Background
  AnimatePacked
  CloseGraph
;

cr .( GotSK3: MoveSprite SpriteDemo ) cr

