
\ ---[ Code for Graphics of the First Kind ]----------[11/29/2006]---
\ The only external file required for this is the RND.4 module, that
\ should be located in the \OS2FORTH\FORTH\APPS directory.  If you
\ wish to use your own Random Number Generator, then feel free to
\ load it in and use it instead.
\ -------------------------------------------------------------------
\ This code is meant for use with the 32Forth system.
\ This is the DOS DPMI version of the compiler in Rick van Norman's
\ OS2FORTH.ZIP package, available on the Taygeta Scientific site.
\ -------------------------------------------------------------------
\ The high level Forth code should work on any other Forth system, if
\ the CODE primitives have been converted to work correctly.
\ -------------------------------------------------------------------

exists [CLFCode] [if]
  forget [CLFCode]
[then]

: [CLFCode] ;

code SetMode ( mode -- )
                                \ mode in bx on entry as TOS
                 ax ax  xor     \ function #0
                 bl al  mov     \ mode to AL
              INT10 #)  call
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

: InitGraph ( mode# -- ) SetMode ;
: CloseGraph ( -- )    3 SetMode ;


code Plot ( x y c -- )
                                \ c in bx on entry as TOS
                 bl al  mov     \ al=pixel color
                    dx  pop     \ dx=y
                    cx  pop     \ cx=x
                 bx bx  xor     \ bx=page#
               12 # ah  mov     \ ah=Plot Pixel function
              INT10 #)  call    \ do it
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

include \os2forth\forth\apps\rnd.4

: TestPixels ( -- )
  $13 InitGraph         \ sets VGA 320x200x256 mode
  begin
    key? not            \ loop until a key is pressed
  while
    319 rnd             \ get a random X coord [0..319]
    199 rnd             \ get a random Y coord [0..199]
    255 rnd             \ get a random Color   [0..255]
    Plot
  repeat
  key drop              \ lose the keypress
  CloseGraph            \ sets 80x25 Text mode
;

: VLine ( x y1 y2 c -- )
  -rot                  \ x c y1 y2
  over                  \ x c y1 y2 y1
  - 1+                  \ x c y1 len
  0 do                  \ x c y1
    2 pick              \ x c y1 x
    over                \ x c y1 x y1
    i +                 \ x c y1 x y
    3 pick              \ x c y1 x y c
    Plot                \ x c y1
  loop                  \ x c y1
  2drop                 \ x
  drop                  \ --
;

: HLine ( x1 x2 y c -- )
  -rot                  \ x1 c x2 y
  swap                  \ x1 c y x2
  3 pick                \ x1 c y x2 x1
  - 1+                  \ x1 c y len
  0 do                  \ x1 c y
    2 pick              \ x1 c y x1
    i +                 \ x1 c y x
    over                \ x1 c y x y
    3 pick              \ x1 c y x y c
    Plot                \ x1 c y
  loop
  2drop                 \ x
  drop                  \ --
;

: TestLines ( -- )
  $13 InitGraph
  \ draw a border to the screen
    0 319   0 14 HLine
    0 319 199 14 HLine
    0   0 199 14 VLine
  319   0 199 14 VLine
  \ draw a pair of filled rectangles
  50 0 do
    50 100 i 10 + 15 HLine
    150 i + 100 150 1 VLine
  loop
  key drop
  CloseGraph
;

: Box ( x1 y1 x2 y2 c -- )
  \ draw top of box
  4 pick                \ x1 y1 x2 y2 c x1
  3 pick                \ x1 y1 x2 y2 c x1 x2
  5 pick                \ x1 y1 x2 y2 c x1 x2 y1
  3 pick                \ x1 y1 x2 y2 c x1 x2 y1 c
  HLine                 \ x1 y1 x2 y2 c
  \ draw bottom of box
  4 pick                \ x1 y1 x2 y2 c x1
  3 pick                \ x1 y1 x2 y2 c x1 x2
  3 pick                \ x1 y1 x2 y2 c x1 x2 y2
  3 pick                \ x1 y1 x2 y2 c x1 x2 y2 c
  HLine                 \ x1 y1 x2 y2 c
  \ draw left side of box
  4 pick                \ x1 y1 x2 y2 c x
  4 pick                \ x1 y1 x2 y2 c x y1
  3 pick                \ x1 y1 x2 y2 c x y1 y2
  3 pick                \ x1 y1 x2 y2 c x y1 y2 c
  VLine                 \ x1 y1 x2 y2 c
  \ draw right side of box
  >R                    \ x1 y1 x2 y2
  >R                    \ x1 y1 x2
  swap                  \ x1 x2 y1
  R>                    \ x1 x2 y1 y2
  R>                    \ x1 x2 y1 y2 c
  VLine                 \ x1
  drop                  \ --
;

: FBox ( x1 y1 x2 y2 c -- )
  swap                  \ x1 y1 x2 c y2
  3 pick                \ x1 y1 x2 c y2 y1
  - 1+                  \ x1 y1 x2 c h
  0 do                  \ x1 y1 x2 c
    3 pick              \ x1 y1 x2 c x1
    2 pick              \ x1 y1 x2 c x1 x2
    4 pick              \ x1 y1 x2 c x1 x2 y1
    i +                 \ x1 y1 x2 c x1 x2 y
    3 pick              \ x1 y1 x2 c x1 x2 y c
    HLine               \ x1 y1 x2 c
  loop                  \ x1 y1 x2 c
  2drop                 \ x1 y1
  2drop                 \ --
;

: TestBoxes ( -- )
  $13 InitGraph
    0   0 319 199 14 Box
   10 100  60 130 15 Box
   10 150  60 190  1 Box
   10  10  60  60 13 FBox
  100  10 150  60 12 FBox
  200  10 300 150 11 FBox
  key drop
  CloseGraph
;

\ ---[ End of code ]---

