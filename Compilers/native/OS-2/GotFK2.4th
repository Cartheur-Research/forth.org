\ ---[ Code Addendum 02 ]-----------------------------[11/29/2006]---
\
\          For the Graphics of the First Kind Part II column
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
\ Save as CLFCODE2.4TH in your \os2forth\forth directory

exists [CLFCODE2] [if]
  forget [CLFCODE2]
[then]

: [CLFCODE2] ;

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

: VLine ( x y1 y2 c -- )
  -rot over - 1+                \ x c y1 len
  0 do                          \ x c y1
    2 pick over i + 3 pick Plot \ x c y1
  loop                          \ x c y1
  2drop                         \ x
  drop                          \ --
;

: HLine ( x1 x2 y c -- )
  -rot swap 3 pick - 1+         \ x1 c y len
  0 do                          \ x1 c y
    2 pick i + over 3 pick Plot \ x1 c y
  loop                          \ x1 c y
  2drop                         \ x
  drop                          \ --
;

[then]

\ ---[ Code for CLFCODE2.4TH ]---------------------------------------

value VSelector $0A000 SEG>DESC to VSelector

code Plot1 ( c ofs -- )
                                \ bx=ofs on entry as TOS
                    cx  pop     \ cx=c
                    es  push    \ save registers to modify
                    di  push
                 bx di  mov     \ di=ofs
        VSelector # ax  mov     \ ax=selector
                 ax es  mov     \ es=selector
                 cl al  mov     \ al=c
         es: al 0 [di]  mov     \ plot the pixel
                    di  pop     \ restore registers
                    es  pop
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

: TestPlot1 ( -- )
  $13 InitGraph
  begin
    key? not
  while
    255 rnd                     \ get a random Color   [0..255]
    319 rnd                     \ get a random X coord [0..319]
    199 rnd                     \ get a random Y coord [0..199]
    320 * +                     \ calculate Y*320+X address offset
    Plot1
  repeat
  key drop                      \ lose the keypress
  CloseGraph                    \ sets 80x25 Text mode
;

code Y*320 ( y -- y*320 )
                                \ bx=y on entry as TOS
                6 # cl  mov
                 bx cl  shl     \ bx=y*64
                 bx ax  mov     \ ax=y*64
                2 # cl  mov
                 ax cl  shl     \ ax=y*256  two more bit positions
                 ax bx  add     \ bx=y*320  256+64=320...
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

variable WhichPlot

: toggleit ( vadr -- ) dup @ not swap ! ;

: TestPlots
  0 WhichPlot !                 \ Start off slow
  $13 InitGraph
  begin
    key? if
      key
      dup 27 = if               \ ESC exits
        drop 1
      else
        32 = if                 \ space bar toggles
          WhichPlot toggleit
          $13 InitGraph         \ clears the screen
          0
        then
      then
    else
      0
    then
    not
  while
    319 rnd                     \ get a random X coord [0..319]
    199 rnd                     \ get a random Y coord [0..199]
    255 rnd                     \ get a random Color   [0..255]
    WhichPlot @ if
      FastPlot
    else
      Plot
    then
  repeat
  CloseGraph                    \ sets 80x25 Text mode
;

code FastVLine ( x y1 y2 c -- )
                                \ c in bx on entry as TOS
                    bx  push    \ put all parameters onto the stack
                 bp sp  xchg    \ setup the stack frame
                    es  push    \ save registers to be used
                    di  push
        VSelector # ax  mov     \ es=selector
                 ax es  mov
       \ Calculate Y*320+X
       3 cells [bp] di  mov     \ di=x
       2 cells [bp] ax  mov     \ ax=y1
                6 # cl  mov
                 ax cl  shl     \ ax=y1*64
                 ax di  add     \ di=y*64+x
                2 # cl  mov
                 ax cl  shl     \ ax=y*256
                 ax di  add     \ di=y*320+x
       1 cells [bp] ax  mov     \ ax=y2
       2 cells [bp] ax  sub     \ ax=y2-y1
                    ax  inc
                 ax cx  mov     \ cx=len
       0 cells [bp] al  mov     \ al=c
1 L:
         es: al 0 [di]  mov     \ plot the pixel
              320 # di  add     \ increment to next line
                    cx  dec     \ decrement count
                  1 L#  jnz     \ loop until 0

                    di  pop     \ restore register
                    es  pop
                 bp sp  xchg    \ restore stack frame
          4 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

code FastHLine ( x1 x2 y c -- )
                                \ c in bx on entry as TOS
                    bx  push    \ put all parameters onto the stack
                 bp sp  xchg    \ setup the stack frame
                    es  push    \ save registers to be used
                    di  push
        VSelector # ax  mov     \ es=selector
                 ax es  mov
       \ Calculate Y*320+X
       3 cells [bp] di  mov     \ di=x
       1 cells [bp] ax  mov     \ ax=y
                6 # cl  mov
                 ax cl  shl     \ ax=y1*64
                 ax di  add     \ di=y*64+x
                2 # cl  mov
                 ax cl  shl     \ ax=y*256
                 ax di  add     \ di=y*320+x
       \ Calculate Length
       2 cells [bp] ax  mov     \ ax=x2
       3 cells [bp] ax  sub     \ ax=x2-x1
                    ax  inc
                 ax cx  mov     \ cx=len
       0 cells [bp] al  mov     \ al=c
                        cld     \ set forward direction for DI inc
                        rep
                    al  stosb

                    di  pop     \ restore register
                    es  pop
                 bp sp  xchg    \ restore stack frame
          4 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

variable WhichDir
variable cflag

: +cflag                        \ limits colors to range [0..15]
  1 cflag +!
  cflag @ 15 > if 1 cflag ! then
;

: TestLines2
  0 WhichPlot !                 \ 0=Slow, 1=Fast
  0 WhichDir !                  \ 0=Hor,  1=Ver
  0 cflag !
  $13 InitGraph
  begin
    key? if
      key
      dup 27 = if               \ ESC exits
        drop 1
      else
        dup 32 = if             \ space bar toggles speed
          WhichPlot toggleit
          0
        else
          8 = if                \ bckspc toggles direction h/v
            WhichDir toggleit
            0
          then
        then
      then
    else
      0
    then
    not
  while
    +cflag
    WhichDir @ if 320 else 200 then
    0 do
      WhichDir @ if
        i 10 190 cflag @
      else
        20 300 i cflag @
      then
      WhichDir @ if
        WhichPlot @ if
          VLine
        else
          FastVLine
        then
      else
        WhichPlot @ if
          HLine
        else
          FastHLine
        then
      then
    loop
  repeat
  CloseGraph
;

\ ---[ End of CLFCODE2.4TH ]-----------------------------------------

