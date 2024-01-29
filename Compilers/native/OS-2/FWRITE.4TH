\ ===================================================================
\      Program: FWrite.4th
\       Author: Timothy Trussell
\         Date: 09/07/2006 -- Conversion to 32Forth
\  Description: Text screen output module
\ Forth System: 32FORTH by Rick van Norman, available on Taygeta
\    Assembler: Built-in FORTH assembler
\ ===================================================================
\ 32Forth is what I am calling the DOS DPMI version of Rick's Forth
\ system in his OS2FORTH.ZIP package, which can be found at
\
\                        www.taygeta.com
\
\ ===================================================================

\ ---[ FWrite ]---------------------------------------------------
\        Fast Text Screen Writing Unit for the 32Forth System
\ -------------------------------------------------------------------
\ The only change needed to make this work with other version of the
\ Forth language is the coding of the FastC word.
\ -------------------------------------------------------------------
\ To Do's: Add the Shadow function to the Box creation routines.
\ -------------------------------------------------------------------

\ Requires: EDO, ENUM

\ ---[ Prototypes ]--------------------------------------------------
\
\ code FastC    ( x y attr c -- )
\ : FastS       ( x y attr &str len -- )
\ : Attr        ( fg bg -- attr )
\ : ClearText   ( x1 y1 x2 y2 fg bg -- )
\ : ClearLine   ( y f b -- )
\ : Box         ( x1 y1 x2 y2 fg bg btype -- )
\ : FBox        ( x1 y1 x2 y2 f b boxtype -- )
\ : HorizLine   ( x1 x2 y fg bg ltype -- _
\ : VertLine    ( x y1 y2 fg bg ltype -- )
\ : WriteAT     ( x y fg bg &str len -- )
\ : WriteCenter ( y fg bg &str len -- )
\ : BackScreen  ( -- )

exists [FWRITE] not [if]      \ if loaded, skip

: [FWRITE] ;

decimal

\ ---[ EDO by GT Hawkins ]-------------------------------------------
\ This is on the Taygeta Scientific site.  It is a basic data
\ structure package.

1 CONSTANT 1BYTE

cell CONSTANT 1WORD ( -- n )

: BYTE* ;
: WORD+  ( n -- n+WORD )  1WORD + ;
: WORD*  ( n -- n*WORD )  1WORD * ;

: DEF   ( size -- )       CONSTANT ;

: S{    ( -- 0 )          0 ;

: ::    ( offset object-definition -- offset )
  CREATE OVER , + DOES> @ + ;

: }S    ( size -- )       CONSTANT ;

: []    ( object-definition #objects -- )
  OVER * CONSTANT
  CREATE ,
  DOES> @ * + ;

\ ---[Note]----------------------------------------------------------
\         These are the additions I have made to the wordset
\ ------------------------------------------------------[End Note]---

8 constant 1FLOAT
: FLOAT* ( n -- n*FLOAT ) FLOATS ;

  1BYTE DEF {1BYTE}-DEF
2 BYTE* DEF {2BYTE}-DEF
4 BYTE* DEF {4BYTE}-DEF
  1WORD DEF {1WORD}-DEF
2 WORD* DEF {2WORD}-DEF
8 BYTE* DEF {1FLOAT}-DEF

\ ---[ ENUM ]--------------------------------------------------------
\ This is also on the Taygeta Scientific site

: ENUM+ ( n -- ) create , does> ( -- n ) dup @ constant 1 swap +! ;
: ENUM  ( -- )   0 ENUM+ ;

\ ---[ Modulus Startus Upus ]----------------------------------------

enum [color]
[color] Black
[color] Blue
[color] Green
[color] Cyan
[color] Red
[color] Magenta
[color] Brown
[color] LightGray
[color] DarkGray
[color] LightBlue
[color] LightGreen
[color] LightCyan
[color] LightRed
[color] LightMagenta
[color] Yellow
[color] White

\ ---[ TSelector ]---------------------------------------------------
\ We need to directly access the $0B800 segment in the first megabyte
\ address space on the computer, and to do so we have to use a 32-bit
\ Selector, which is put into ES instead of the $0B800 segment addr.

value TSelector $0B800 SEG>DESC to TSelector

\ ---[ FastC ]-------------------------------------------------------
\ FastC prints a character and sets the passed attribute at the
\ specified x/y coordinate on the screen.
\ It is to be remembered that the video screen is composed of an
\ 80x25 matrix, where each of the 80 columns consists of two bytes:
\ the first byte is the character displayed on the screen, and the
\ second byte is the foreground/background color attribute byte for
\ that character.
\
\ A typical call to FastC is:
\
\       10 15 White Blue Attr $41 FastC
\
\ which prints and "A" at 10,15 in white with a blue background
\
\ Attr takes the constant values for White and Blue and combines them
\ into a single attribute byte.
\ -------------------------------------------------------------------

code FastC ( x y attr c -- )
                    bx  push
                 bp sp  xchg
                    es  push
                    di  push
        TSelector # ax  mov
                 ax es  mov     \ text selector to es
       3 cells [bp] ax  mov     \ ax=x
                    ax  shl     \ ax=x*2
                 ax di  mov     \ di=x*2
       2 cells [bp] ax  mov     \ ax=y
                5 # cl  mov
                 ax cl  shl     \ ax=y*32
                 ax di  add     \ di=x*2+y*32
                2 # cl  mov
                 ax cl  shl     \ ax=y*128
                 ax di  add     \ di=x*2+y*160
       0 cells [bp] al  mov     \ al=c
         es: al 0 [di]  mov     \ print character
                    di  inc
       1 cells [bp] al  mov     \ al=attr
         es: al 0 [di]  mov     \ set attribute byte
                    di  pop     \ restore registers
                    es  pop
                 bp sp  xchg
               16 # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

\ ---[ FastS ]-------------------------------------------------------
\ Print a string to the display at the specified x/y coords in the
\ color specified by the combined attr byte.
\
\ Note that in a normal calling sequence, such as:
\
\       10 15 White Blue Attr " Kitchen Sink Development" FastS
\
\ the string specifier places the address of the string to be printed
\ followed by it's length onto the stack.
\ -------------------------------------------------------------------

: FastS ( x y attr &str len -- )
  0 do                          \ x y attr %str
    3 pick i +                  \ x y attr %str x+i
    3 pick                      \ x y attr %str x+i y
    3 pick                      \ x y attr %str x+i y attr
    3 pick i + C@               \ x y attr %str x+i y attr c
    FastC                       \ x y attr %str
  loop                          \ x y attr %str
  2drop                         \ x y
  2drop                         \ --
;

\ ---[ Attr ]--------------------------------------------------------
\ Combines the foreground and background colors into a single byte
\ Needed for calling the FastC and FastS routines

: Attr  ( fg bg -- attr )
  255 AND
  4 LSHIFT
  swap
  255 AND
  OR
;

\ ---[Note]----------------------------------------------------------
\ %FillChar is an 81 byte string array - length byte followed by 80
\ char bytes.  It is used by the ClearText function to fill a part of
\ the screen with whatever attribute is specified, and the BL char.
\ To use it, ClearText calculates the length of the window line to be
\ cleared (x2-x1+1) and sets the length byte of %FillChar with it.
\ We pre-fill the array with the space (0x20) character, so it really
\ never needs to be changed.  Only the length byte needs be modified.
\ ------------------------------------------------------[End Note]---

create %FillChar 80 C, here 80 dup allot BL fill

\ ---[ ClearText ]---------------------------------------------------
\ ClearText clears a box on the text screen, filling it with spaces
\ with the specified background color

: ClearText ( x1 y1 x2 y2 fg bg -- )
  Attr                          \ x1 y1 x2 y2 attr
  \ if x2>80 then make x2=80
  2 pick 80 > if                \ x1 y1 x2 y2 attr
    >R >R drop 80 R> R>         \ x1 y1 x2 y2 attr
  then                          \ x1 y1 x2 y2 attr
  swap                          \ x1 y1 x2 attr y2
  >R                            \ x1 y1 x2 attr
  rot                           \ x1 x2 attr y1
  R>                            \ x1 x2 attr y1 y2
  1+                            \ x1 x2 attr y1 y2+1
  swap                          \ x1 x2 attr y2+1 y1
  do                            \ x1 x2 attr
    2 pick                      \ x1 x2 attr x1
    i                           \ x1 x2 attr x1 i
    2 pick                      \ x1 x2 attr x1 i attr
    %FillChar 1+                \ x1 x2 attr x1 i attr &fc[1]
    5 pick                      \ x1 x2 attr x1 i attr &fc[1] x2
    4 pick                      \ x1 x2 attr x1 i attr &fc[1] x2 x1
    -                           \ x1 x2 attr x1 i attr &fc[1] x2-x1
    1+                          \ x1 x2 attr x1 i attr &fc[1] len
    FastS                       \ x1 x2 attr
  loop                          \ x1 x2 attr
  2drop drop                    \ --
;

\ ---[ ClearLine ]---------------------------------------------------
\ Clears a line on the screen to spaces, using the specified color
\ for the background
\
\ Example:
\               24 White Black ClearLine

: ClearLine ( y f b -- )
  Attr                          \ y attr
  0 -rot                        \ 0 y attr
  %FillChar 1+                  \ 0 y attr &fc[1]
  80                            \ 0 y attr &fc[1] 80
  FastS                         \ --
;

\ ---[ BoxFrame ]----------------------------------------------------
\ A structure definition for accessing the WFrame[] array that holds
\ the characters used for the Boxes that can be drawn to the screen

S{
  {1BYTE}-DEF :: .TL          \ top left
  {1BYTE}-DEF :: .TM          \ top middle
  {1BYTE}-DEF :: .TR          \ top right
  {1BYTE}-DEF :: .LS          \ left side
  {1BYTE}-DEF :: .RS          \ right side
  {1BYTE}-DEF :: .BL          \ bottom left
  {1BYTE}-DEF :: .BM          \ bottom middle
  {1BYTE}-DEF :: .BR          \ bottom right
}S BoxFrame-DEF

BoxFrame-DEF 14 [] BoxFrame[]-OBJ BoxFrame-NDX

\ ---[Note]----------------------------------------------------------
\ I have to do it this way, as the correct characters will NOT
\ display properly in my editor program.
\ Oh, for the OLD days when I was using QEdit v2.15...
\ ------------------------------------------------------[End Note]---

create WFrame[]

\ TL    TM     TR     LS     RS     BL     BM     BR
 32 C,  32 C,  32 C,  32 C,  32 C,  32 C,  32 C,  32 C, \ no frame
218 C, 196 C, 191 C, 179 C, 179 C, 192 C, 196 C, 217 C, \ single line box
201 C, 205 C, 187 C, 186 C, 186 C, 200 C, 205 C, 188 C, \ double line box
213 C, 205 C, 184 C, 179 C, 179 C, 212 C, 205 C, 190 C, \ dbl top/bot, single sides
218 C, 196 C, 183 C, 179 C, 186 C, 212 C, 205 C, 188 C, \ sng top/left, dbl right/bot
201 C, 205 C, 184 C, 186 C, 179 C, 211 C, 196 C, 217 C, \ dbl top/left, sng right/bot
213 C, 205 C, 187 C, 179 C, 186 C, 192 C, 196 C, 189 C, \ dbl top/right, sng left/bot
214 C, 196 C, 191 C, 186 C, 179 C, 200 C, 205 C, 190 C, \ sng top/right, dbl left/bot
219 C, 219 C, 219 C, 219 C, 219 C, 219 C, 219 C, 219 C, \ Solid blocks
219 C, 223 C, 219 C, 219 C, 219 C, 219 C, 220 C, 219 C, \ Solid sides, half top/bot
222 C, 223 C, 221 C, 222 C, 221 C, 222 C, 220 C, 222 C, \ half solids all around
178 C, 178 C, 178 C, 178 C, 178 C, 178 C, 178 C, 178 C, \ light shaded
177 C, 177 C, 177 C, 177 C, 177 C, 177 C, 177 C, 177 C, \ medium shaded
176 C, 176 C, 176 C, 176 C, 176 C, 176 C, 176 C, 176 C, \ dark shaded

value %BoxType

\ ---[ Box ]---------------------------------------------------------
\ Draws a box (window) onto the text screen.
\ I am not allowing the user to pass a character to be used for the
\ frame in this version (at this time)

: Box ( x1 y1 x2 y2 fg bg btype -- )
 \ if boxtype > 14 OR < 0 then set it to type 0
  dup 14 > over 0 <             \ x1 y1 x2 y2 fg bg btype 0/1 0/1
  or if                         \ x1 y1 x2 y2 fg bg btype
    drop 0                      \ x1 y1 x2 y2 fg bg 0
  then                          \ x1 y1 x2 y2 fg bg btype
  to %BoxType                   \ x1 y1 x2 y2 fg bg
  Attr                          \ x1 y1 x2 y2 attr
 \ print top left corner of box
  4 pick                        \ x1 y1 x2 y2 attr x1
  4 pick                        \ x1 y1 x2 y2 attr x1 y1
  2 pick                        \ x1 y1 x2 y2 attr x1 y1 attr
  WFrame[]                      \ x1 y1 x2 y2 attr x1 y1 attr &WFrame[]
  %BoxType                      \ x1 y1 x2 y2 attr x1 y1 attr &WFrame[] btype
  BoxFrame-NDX                  \ x1 y1 x2 y2 attr x1 y1 attr &WFrame[btype]
  .TL C@                        \ x1 y1 x2 y2 attr x1 y1 attr TL
  FastC                         \ x1 y1 x2 y2 attr
 \ print top line of box
  2 pick                        \ x1 y1 x2 y2 attr x2
  5 pick                        \ x1 y1 x2 y2 attr x2 x1
  - 1-                          \ x1 y1 x2 y2 attr len
  0 do                          \ x1 y1 x2 y2 attr len
    4 pick 1+ i +               \ x1 y1 x2 y2 attr x1+i
    4 pick                      \ x1 y1 x2 y2 attr x1+i y1
    2 pick                      \ x1 y1 x2 y2 attr x1+i y1 attr
    WFrame[]
    %BoxType
    BoxFrame-NDX
    .TM C@                      \ x1 y1 x2 y2 attr x1+i y1 attr c
    FastC                       \ x1 y1 x2 y2 attr
  loop
 \ print top right corner
  2 pick                        \ x1 y1 x2 y2 attr x2
  4 pick                        \ x1 y1 x2 y2 attr x2 y2
  2 pick                        \ x1 y1 x2 y2 attr x2 y2 attr
  WFrame[]
  %BoxType
  BoxFrame-NDX
  .TR C@                        \ x1 y1 x2 y2 attr x2 y2 attr c
  FastC                         \ x1 y1 x2 y2 attr
 \ print left/right sides of box
  \ I'm not subtracting 1 from y2, because - since it's the end loop
  \ value, the loop will stop 1 short of it anyways...
  over 4 pick 1+ do             \ x1 y1 x2 y2 attr
    4 pick                      \ x1 y1 x2 y2 attr x1
    i                           \ x1 y1 x2 y2 attr x1 i
    2 pick                      \ x1 y1 x2 y2 attr x1 i attr
    WFrame[]
    %BoxType
    BoxFrame-ndx
    .LS C@                      \ x1 y1 x2 y2 attr x1 i attr c
    FastC                       \ x1 y1 x2 y2 attr
    2 pick                      \ x1 y1 x2 y2 attr x2
    i                           \ x1 y1 x2 y2 attr x2 i
    2 pick                      \ x1 y1 x2 y2 attr x2 i attr
    WFrame[]
    %BoxType
    BoxFrame-ndx
    .RS C@                      \ x1 y1 x2 y2 attr x2 i attr c
    FastC                       \ x1 y1 x2 y2 attr
  loop
 \ print bottom left corner
  4 pick                        \ x1 y1 x2 y2 attr x1
  2 pick                        \ x1 y1 x2 y2 attr x1 y2
  2 pick                        \ x1 y1 x2 y2 attr x1 y2 attr
  WFrame[]
  %BoxType
  BoxFrame-NDX
  .BL C@                        \ x1 y1 x2 y2 attr x1 y2 attr c
  FastC                         \ x1 y1 x2 y2 attr
 \ print bottom line of box
  2 pick                        \ x1 y1 x2 y2 attr x2
  5 pick                        \ x1 y1 x2 y2 attr x2 x1
  - 1-                          \ x1 y1 x2 y2 attr len
  0 do                          \ x1 y1 x2 y2 attr len
    4 pick 1+ i +                  \ x1 y1 x2 y2 attr x1+i
    2 pick                      \ x1 y1 x2 y2 attr x1+i y2
    2 pick                      \ x1 y1 x2 y2 attr x1+i y2 attr
    WFrame[]
    %BoxType
    BoxFrame-NDX
    .BM C@                      \ x1 y1 x2 y2 attr x1+i y2 attr c
    FastC                       \ x1 y1 x2 y2 attr
  loop
 \ print bottom right corner
  WFrame[]
  %BoxType
  BoxFrame-NDX
  .BR C@                        \ x1 y1 x2 y2 attr c
  FastC                         \ x1 y1
  2drop                         \ --
;

\ ---[ FBox ]--------------------------------------------------------
\ Draws a box, and then clears it to the background color
\
\ Example:
\               10 5 70 20 White Cyan 5 FBox

: FBox                          ( x1 y1 x2 y2 f b boxtype -- )
  >R 5 pick 5 pick 5 pick
  5 pick 5 pick 5 pick R>       ( x1 y1 x2 y2 f b x1 y1 x2 y2 f b boxtype )
  Box                           ( x1 y1 x2 y2 f b )
  >R >R                         ( x1 y1 x2 y2 )                    ( b f )
  1 - >R                        ( x1 y1 x2 )                  ( b f y2-1 )
  1 - >R                        ( x1 y1 )                ( b f y2-1 x2-1 )
  1 + >R                        ( x1 )              ( b f y2-1 x2-1 y1+1 )
  1 +                           ( x1+1 )            ( b f y2-1 x2-1 y1+1 )
  R>                            ( x1+1 y1+1 )            ( b f y2-1 x2-1 )
  R>                            ( x1+1 y1+1 x2-1 )            ( b f y2-1 )
  R>                            ( x1+1 y1+1 x2-1 y2-1 )            ( b f )
  R> R>                         ( x1+1 y1+1 x2-1 y2-1 f b )
  ClearText                     ( -- )
;

value %LineType

\ ---[ HorizLine ]---------------------------------------------------
\ Draws either a single or double horizontal line of char 205 or 196

: HorizLine ( x1 x2 y fg bg ltype -- _
  dup 2 =
  swap 4 = or
  if 205 else 196 then
  to %LineType                  \ x1 x2 y fg bg
  Attr                          \ x1 x2 y attr
  rot                           \ x1 y attr x2
  3 pick                        \ x1 y attr x2 x1
  -  1+                         \ x1 y attr len
  0 do                          \ x1 y attr
    2 pick i +                  \ x1 y attr x1+i
    2 pick                      \ x1 y attr x1+i y
    2 pick                      \ x1 y attr x1+i y attr
    %LineType                   \ x1 y attr x1+i y attr c
    FastC                       \ x1 y attr
  loop
  2drop
  drop
;

\ ---[ VertLine ]----------------------------------------------------
\ Draws either a single or double vertical line of char 186 or 179

: VertLine ( x y1 y2 fg bg ltype -- )
  dup 2 =
  swap 4 = or
  if 186 else 179 then
  to %LineType
  Attr                          \ x y1 y2 attr
  swap                          \ x y1 attr y2
  2 pick                        \ x y1 attr y2 y1
  - 1+                          \ x y1 attr len
  0 do                          \ x y1 attr
    2 pick
    2 pick i +
    2 pick
    %LineType
    FastC
  loop
  2drop
  drop
;

\ ---[ WriteAT ]-----------------------------------------------------
\ Writes the string at the specified coordinates
\
\ Example:
\          20 3 White Cyan " The Future is FORTH" WriteAT

: WriteAT ( x y fg bg &str len -- )
  >R >R
  Attr
  R> R>
  FastS

;

\ ---[ WriteCenter ]-------------------------------------------------
\ Writes the string, centered on the specified line
\
\  Example:
\           24 Yellow Black " Press Any Key" WriteCenter

: WriteCenter ( y fg bg &str len -- )
  >R >R
  Attr
  R> R>
  40                            \ y attr &str len 40
  over                          \ y attr &str len 40 len
  2 /                           \ y attr &str len 40 len/2
  -                             \ y attr &str len x
  -rot                          \ y attr x &str len
  >R >R                         \ y attr x
  -rot                          \ x y attr
  R> R>                         \ x y attr &str len
  FastS                         \ --
;

\ ---[ BackScreen ]--------------------------------------------------
\ This is my preferred background screen pattern when I'm putting
\ text windows on the screen.

: BackScreen ( -- )
  0 0 LightGray Black Attr 201 FastC
  78 1 do
    i 0 LightGray Black Attr 203 FastC
  loop
  78 0 LightGray Black Attr 187 FastC
  23 1 do
    0 i LightGray Black Attr 204 FastC
    78 1 do
      i j LightGray Black Attr 206 FastC
    loop
    78 i LightGray Black Attr 185 FastC
  loop
  0 23 LightGray Black Attr 200 FastC
  78 1 do
    i 23 LightGray Black Attr 202 FastC
  loop
  78 23 LightGray Black Attr 188 FastC
;

[then]

