\ ---[ Code Addendum 05 ]-----------------------------[12/19/2006]---
\
\          For the Graphics of the Second Kind Part V column
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
\ Save as GotSK5.4th in your \os2forth\forth directory

exists [GotSK5] [if]
  forget [GotSK5]
[then]

: [GotSK5] ;

\ ---[Note]----------------------------------------------------------
\       This file is stand-alone from the other Code Addendums
\ ------------------------------------------------------[End Note]---

decimal

\ ---[ Library Modules ]---------------------------------------------

exists SetMode not [if]

code SetMode ( mode -- )
                 bx ax  mov
              int10 #)  call
                    bx  pop
                    end-code
                    NO-EXPAND

: InitGraph ( mode -- ) SetMode ;
: CloseGraph ( -- )   3 SetMode ;

[then]

exists WaitRetrace not [if]

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
[then]

: ENUM+ ( n -- ) create , does> ( -- n ) dup @ constant 1 swap +! ;
: ENUM  ( -- )   0 ENUM+ ;

\ ---[ Data Definitions ]--------------------------------------------

\ need to see how to release this selector after it's been used.

value fselector $0f000 seg>desc to fselector

exists VSelector not [if]
  value VSelector $0A000 SEG>DESC to VSelector
[then]

\ In 320x200x256, we obviously have lots more colors to choose from.

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

\ ---[ TextWidth ]---------------------------------------------------
\ Used to specify the 8x8 text width of the screen
\ In 320x200, this is 40 characters.  In 640x480, this is 80.
\ This allows for easier updating when we get to implementing the
\ higher resolutions.

40 constant TextWidth

\ ---[ System Font Data ]--------------------------------------------
\ I wasn't going to include this, but when I ran this demo on my
\ laptop, it came up with garbage characters after I grabbed the data
\ at F000h:FA6Eh, where the Font data is located on a 100% PC
\ compatible.  Obviously, my laptop isn't.  So, I include this data
\ set of a typical BIOS font image set, of 128 characters.
\ I have left the LoadBIOSFontData word in place, so you may try it
\ yourself on your computer.  Just remove the comment from the
\ LoadBIOSFontData at the bottom of this listing to enable it.
\ -- Or just type LoadBIOSFontData from the ok prompt and then run
\ the demo ShowCharSet to see if the data is present on your computer
\ -------------------------------------------------------------------
\ These characters are laid out like how I described my Packed Pixel
\ Sprite Column.
\
\ 078 C, 0CC C, 00C C, 038 C, 00C C, 0CC C, 078 C, 000 C, \ 33 3
\
\       01111000
\       11001100
\       00001100
\       00111000
\       00001100
\       11001100
\       01111000
\       00000000
\
\ So my basic Packed Pixel sprite routine, modified to handle the 8x8
\ pixel format, will work perfectly here.
\ -------------------------------------------------------------------

base @ hex

create SystemFont[]
000 C, 000 C, 000 C, 000 C, 000 C, 000 C, 000 C, 000 C, \ 00
07E C, 081 C, 0A5 C, 081 C, 0BD C, 099 C, 081 C, 07E C, \ 01
07E C, 0FF C, 0DB C, 0FF C, 0C3 C, 0E7 C, 0FF C, 07E C, \ 02
06C C, 0FE C, 0FE C, 0FE C, 07C C, 038 C, 010 C, 000 C, \ 03
010 C, 038 C, 07C C, 0FE C, 07C C, 038 C, 010 C, 000 C, \ 04
038 C, 07C C, 038 C, 0FE C, 0FE C, 07C C, 038 C, 07C C, \ 05
010 C, 010 C, 038 C, 07C C, 0FE C, 07C C, 038 C, 07C C, \ 06
000 C, 000 C, 018 C, 03C C, 03C C, 018 C, 000 C, 000 C, \ 07
0FF C, 0FF C, 0E7 C, 0C3 C, 0C3 C, 0E7 C, 0FF C, 0FF C, \ 08
000 C, 03C C, 066 C, 042 C, 042 C, 066 C, 03C C, 000 C, \ 09
0FF C, 0C3 C, 099 C, 0BD C, 0BD C, 099 C, 0C3 C, 0FF C, \ 0A
00F C, 007 C, 00F C, 07D C, 0CC C, 0CC C, 0CC C, 078 C, \ 0B
03C C, 066 C, 066 C, 066 C, 03C C, 018 C, 07E C, 018 C, \ 0C
03F C, 033 C, 03F C, 030 C, 030 C, 070 C, 0F0 C, 0E0 C, \ 0D
07F C, 063 C, 07F C, 063 C, 063 C, 067 C, 0E6 C, 0C0 C, \ 0E
099 C, 05A C, 03C C, 0E7 C, 0E7 C, 03C C, 05A C, 099 C, \ 0F
080 C, 0E0 C, 0F8 C, 0FE C, 0F8 C, 0E0 C, 080 C, 000 C, \ 10
002 C, 00E C, 03E C, 0FE C, 03E C, 00E C, 002 C, 000 C, \ 11
018 C, 03C C, 07E C, 018 C, 018 C, 07E C, 03C C, 018 C, \ 12
066 C, 066 C, 066 C, 066 C, 066 C, 000 C, 066 C, 000 C, \ 13
07F C, 0DB C, 0DB C, 07B C, 01B C, 01B C, 01B C, 000 C, \ 14
03E C, 063 C, 038 C, 06C C, 06C C, 038 C, 0CC C, 078 C, \ 15
000 C, 000 C, 000 C, 000 C, 07E C, 07E C, 07E C, 000 C, \ 16
018 C, 03C C, 07E C, 018 C, 07E C, 03C C, 018 C, 0FF C, \ 17
018 C, 03C C, 07E C, 018 C, 018 C, 018 C, 018 C, 000 C, \ 18
018 C, 018 C, 018 C, 018 C, 07E C, 03C C, 018 C, 000 C, \ 19
000 C, 018 C, 00C C, 0FE C, 00C C, 018 C, 000 C, 000 C, \ 1A
000 C, 030 C, 060 C, 0FE C, 060 C, 030 C, 000 C, 000 C, \ 1B
000 C, 000 C, 0C0 C, 0C0 C, 0C0 C, 0FE C, 000 C, 000 C, \ 1C
000 C, 024 C, 066 C, 0FF C, 066 C, 024 C, 000 C, 000 C, \ 1D
000 C, 018 C, 03C C, 07E C, 0FF C, 0FF C, 000 C, 000 C, \ 1E
000 C, 0FF C, 0FF C, 07E C, 03C C, 018 C, 000 C, 000 C, \ 1F
000 C, 000 C, 000 C, 000 C, 000 C, 000 C, 000 C, 000 C, \ 20 <space>
030 C, 078 C, 078 C, 078 C, 030 C, 000 C, 030 C, 000 C, \ 21 !
06C C, 06C C, 06C C, 000 C, 000 C, 000 C, 000 C, 000 C, \ 22 "
06C C, 06C C, 0FE C, 06C C, 0FE C, 06C C, 06C C, 000 C, \ 23 #
030 C, 07C C, 0C0 C, 078 C, 00C C, 0F8 C, 030 C, 000 C, \ 24 $
000 C, 0C6 C, 0CC C, 018 C, 030 C, 066 C, 0C6 C, 000 C, \ 25 %
038 C, 06C C, 038 C, 076 C, 0DC C, 0CC C, 076 C, 000 C, \ 26 &
060 C, 060 C, 0C0 C, 000 C, 000 C, 000 C, 000 C, 000 C, \ 27 ' tick
018 C, 030 C, 060 C, 060 C, 060 C, 030 C, 018 C, 000 C, \ 28 (
060 C, 030 C, 018 C, 018 C, 018 C, 030 C, 060 C, 000 C, \ 29 )
000 C, 066 C, 03C C, 0FF C, 03C C, 066 C, 000 C, 000 C, \ 2A *
000 C, 030 C, 030 C, 0FC C, 030 C, 030 C, 000 C, 000 C, \ 2B +
000 C, 000 C, 000 C, 000 C, 000 C, 030 C, 030 C, 060 C, \ 2C , comma
000 C, 000 C, 000 C, 0FC C, 000 C, 000 C, 000 C, 000 C, \ 2D -
000 C, 000 C, 000 C, 000 C, 000 C, 030 C, 030 C, 000 C, \ 2E . period
006 C, 00C C, 018 C, 030 C, 060 C, 0C0 C, 080 C, 000 C, \ 2F /
07C C, 0C6 C, 0CE C, 0DE C, 0F6 C, 0E6 C, 07C C, 000 C, \ 30 0
030 C, 070 C, 030 C, 030 C, 030 C, 030 C, 0FC C, 000 C, \ 31 1
078 C, 0CC C, 00C C, 038 C, 060 C, 0CC C, 0FC C, 000 C, \ 32 2
078 C, 0CC C, 00C C, 038 C, 00C C, 0CC C, 078 C, 000 C, \ 33 3
01C C, 03C C, 06C C, 0CC C, 0FE C, 00C C, 01E C, 000 C, \ 34 4
0FC C, 0C0 C, 0F8 C, 00C C, 00C C, 0CC C, 078 C, 000 C, \ 35 5
038 C, 060 C, 0C0 C, 0F8 C, 0CC C, 0CC C, 078 C, 000 C, \ 36 6
0FC C, 0CC C, 00C C, 018 C, 030 C, 030 C, 030 C, 000 C, \ 37 7
078 C, 0CC C, 0CC C, 078 C, 0CC C, 0CC C, 078 C, 000 C, \ 38 8
078 C, 0CC C, 0CC C, 07C C, 00C C, 018 C, 070 C, 000 C, \ 39 9
000 C, 030 C, 030 C, 000 C, 000 C, 030 C, 030 C, 000 C, \ 3A :
000 C, 030 C, 030 C, 000 C, 000 C, 030 C, 030 C, 060 C, \ 3B ;
018 C, 030 C, 060 C, 0C0 C, 060 C, 030 C, 018 C, 000 C, \ 3C <
000 C, 000 C, 0FC C, 000 C, 000 C, 0FC C, 000 C, 000 C, \ 3D =
060 C, 030 C, 018 C, 00C C, 018 C, 030 C, 060 C, 000 C, \ 3E >
078 C, 0CC C, 00C C, 018 C, 030 C, 000 C, 030 C, 000 C, \ 3F ?
07C C, 0C6 C, 0DE C, 0DE C, 0DE C, 0C0 C, 078 C, 000 C, \ 40 @
030 C, 078 C, 0CC C, 0CC C, 0FC C, 0CC C, 0CC C, 000 C, \ 41 A
0FC C, 066 C, 066 C, 07C C, 066 C, 066 C, 0FC C, 000 C, \ 42 B
03C C, 066 C, 0C0 C, 0C0 C, 0C0 C, 066 C, 03C C, 000 C, \ 43 C
0F8 C, 06C C, 066 C, 066 C, 066 C, 06C C, 0F8 C, 000 C, \ 44 D
07E C, 060 C, 060 C, 078 C, 060 C, 060 C, 07E C, 000 C, \ 45 E
07E C, 060 C, 060 C, 078 C, 060 C, 060 C, 060 C, 000 C, \ 46 F
03C C, 066 C, 0C0 C, 0C0 C, 0CE C, 066 C, 03E C, 000 C, \ 47 G
0CC C, 0CC C, 0CC C, 0FC C, 0CC C, 0CC C, 0CC C, 000 C, \ 48 H
078 C, 030 C, 030 C, 030 C, 030 C, 030 C, 078 C, 000 C, \ 49 I
01E C, 00C C, 00C C, 00C C, 0CC C, 0CC C, 078 C, 000 C, \ 4A J
0E6 C, 066 C, 06C C, 078 C, 06C C, 066 C, 0E6 C, 000 C, \ 4B K
060 C, 060 C, 060 C, 060 C, 060 C, 060 C, 07E C, 000 C, \ 4C L
0C6 C, 0EE C, 0FE C, 0FE C, 0D6 C, 0C6 C, 0C6 C, 000 C, \ 4D M
0C6 C, 0E6 C, 0F6 C, 0DE C, 0CE C, 0C6 C, 0C6 C, 000 C, \ 4E N
038 C, 06C C, 0C6 C, 0C6 C, 0C6 C, 06C C, 038 C, 000 C, \ 4F O
0FC C, 066 C, 066 C, 07C C, 060 C, 060 C, 0F0 C, 000 C, \ 50 P
078 C, 0CC C, 0CC C, 0CC C, 0DC C, 078 C, 01C C, 000 C, \ 51 Q
0FC C, 066 C, 066 C, 07C C, 06C C, 066 C, 0E6 C, 000 C, \ 52 R
078 C, 0CC C, 0E0 C, 070 C, 01C C, 0CC C, 078 C, 000 C, \ 53 S
0FC C, 030 C, 030 C, 030 C, 030 C, 030 C, 030 C, 000 C, \ 54 T
0CC C, 0CC C, 0CC C, 0CC C, 0CC C, 0CC C, 0FC C, 000 C, \ 55 U
0CC C, 0CC C, 0CC C, 0CC C, 0CC C, 078 C, 030 C, 000 C, \ 56 V
0C6 C, 0C6 C, 0C6 C, 0D6 C, 0FE C, 0EE C, 0C6 C, 000 C, \ 57 W
0C6 C, 0C6 C, 06C C, 038 C, 038 C, 06C C, 0C6 C, 000 C, \ 58 X
0CC C, 0CC C, 0CC C, 078 C, 030 C, 030 C, 078 C, 000 C, \ 59 Y
0FE C, 006 C, 00C C, 018 C, 030 C, 060 C, 0FE C, 000 C, \ 5A Z
078 C, 060 C, 060 C, 060 C, 060 C, 060 C, 078 C, 000 C, \ 5B [
0C0 C, 060 C, 030 C, 018 C, 00C C, 006 C, 002 C, 000 C, \ 5C \
078 C, 018 C, 018 C, 018 C, 018 C, 018 C, 078 C, 000 C, \ 5D ]
010 C, 038 C, 06C C, 0C6 C, 000 C, 000 C, 000 C, 000 C, \ 5E ^
000 C, 000 C, 000 C, 000 C, 000 C, 000 C, 000 C, 0FF C, \ 5F _
030 C, 030 C, 018 C, 000 C, 000 C, 000 C, 000 C, 000 C, \ 60 '
000 C, 000 C, 078 C, 00C C, 07C C, 0CC C, 076 C, 000 C, \ 61 a
0E0 C, 060 C, 060 C, 07C C, 066 C, 066 C, 0DC C, 000 C, \ 62 b
000 C, 000 C, 078 C, 0CC C, 0C0 C, 0CC C, 078 C, 000 C, \ 63 c
01C C, 00C C, 00C C, 07C C, 0CC C, 0CC C, 076 C, 000 C, \ 64 d
000 C, 000 C, 078 C, 0CC C, 0FC C, 0C0 C, 078 C, 000 C, \ 65 e
038 C, 06C C, 060 C, 0F0 C, 060 C, 060 C, 0F0 C, 000 C, \ 66 f
000 C, 000 C, 076 C, 0CC C, 0CC C, 07C C, 00C C, 0F8 C, \ 67 g
0E0 C, 060 C, 06C C, 076 C, 066 C, 066 C, 0E6 C, 000 C, \ 68 h
030 C, 000 C, 070 C, 030 C, 030 C, 030 C, 078 C, 000 C, \ 69 i
00C C, 000 C, 00C C, 00C C, 00C C, 0CC C, 0CC C, 078 C, \ 6A j
0E0 C, 060 C, 066 C, 06C C, 078 C, 06C C, 0E6 C, 000 C, \ 6B k
070 C, 030 C, 030 C, 030 C, 030 C, 030 C, 078 C, 000 C, \ 6C l
000 C, 000 C, 0CC C, 0FE C, 0FE C, 0D6 C, 0C6 C, 000 C, \ 6D m
000 C, 000 C, 0F8 C, 0CC C, 0CC C, 0CC C, 0CC C, 000 C, \ 6E n
000 C, 000 C, 078 C, 0CC C, 0CC C, 0CC C, 078 C, 000 C, \ 6F o
000 C, 000 C, 0DC C, 066 C, 066 C, 07C C, 060 C, 0F0 C, \ 70 p
000 C, 000 C, 076 C, 0CC C, 0CC C, 07C C, 00C C, 01E C, \ 71 q
000 C, 000 C, 0DC C, 076 C, 066 C, 060 C, 0F0 C, 000 C, \ 72 r
000 C, 000 C, 07C C, 0C0 C, 078 C, 00C C, 0F8 C, 000 C, \ 73 s
010 C, 030 C, 07C C, 030 C, 030 C, 034 C, 018 C, 000 C, \ 74 t
000 C, 000 C, 0CC C, 0CC C, 0CC C, 0CC C, 076 C, 000 C, \ 75 u
000 C, 000 C, 0CC C, 0CC C, 0CC C, 078 C, 030 C, 000 C, \ 76 v
000 C, 000 C, 0C6 C, 0D6 C, 0FE C, 0FE C, 06C C, 000 C, \ 77 w
000 C, 000 C, 0C6 C, 06C C, 038 C, 06C C, 0C6 C, 000 C, \ 78 x
000 C, 000 C, 0CC C, 0CC C, 0CC C, 07C C, 00C C, 0F8 C, \ 79 y
000 C, 000 C, 0FC C, 098 C, 030 C, 064 C, 0FC C, 000 C, \ 7A z
01C C, 030 C, 030 C, 0E0 C, 030 C, 030 C, 01C C, 000 C, \ 7B {
018 C, 018 C, 018 C, 000 C, 018 C, 018 C, 018 C, 000 C, \ 7C |
0E0 C, 030 C, 030 C, 01C C, 030 C, 030 C, 0E0 C, 000 C, \ 7D }
076 C, 0DC C, 000 C, 000 C, 000 C, 000 C, 000 C, 000 C, \ 7E ~
000 C, 010 C, 038 C, 06C C, 0C6 C, 0C6 C, 0FE C, 000 C, \ 7F block
base !

\ ---[ LoadBIOSFontData ]--------------------------------------------
\ This loads the font data from the system BIOS into the SystemFont[]
\ array. The data *MAY NOT* be present on all computers.

code LoadBIOSFontData
                    bx  push
                    es  push
                    di  push
                    ds  push
                    si  push
                    ds  push
                    es  pop
        fselector # ax  mov
                 ax ds  mov
     SystemFont[] # di  mov
           $0FA6E # si  mov
                        cld
              256 # cx  mov
                   rep  movs
                    si  pop
                    ds  pop
                    di  pop
                    es  pop
                    bx  pop
                    end-code
                    no-expand

\ ---[ GPutPixel ]---------------------------------------------------
\ Plots a pixel to the screen.

code GPutPixel ( x y c -- )
                                \ c in bx on entry as TOS
                    bx  push    \ put all parameters onto the stack
                 bp sp  xchg    \ setup the stack frame
                    es  push    \ save registers to be used
                    di  push
        VSelector # ax  mov
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

\ ---[ GFastC ]------------------------------------------------------
\ Prints a character and sets the passed attributes at the
\ specified x/y coordinates on the screen.
\
\ In this routine, the color of the 1 pixels in the font will be set
\ to the FG parameter, and the 0 pixels to the BG parameter.
\
\ If you pass a BG parameter of -1, then GFastC will not plot the 0
\ pixels, but skip them, leaving the current background in place.
\
\ A typical call to GFastC is:
\
\       80 120 White Blue $41 GFastC
\
\ which displays the pixels forming the character in White, with the
\ background pixels being Blue.
\
\ The x/y coordinate range for the GFastC word is the normal range
\ of 0..319 by 0..199.  The x/y parameters are scaled to this range
\ by the words that call GFastC, such as the GFastS.
\
\ It should be noted that the programmer is NOT limited to the 16
\ colors available in text modes, but has the full palette range to
\ work with in this graphics version - 256 in 320x200x256 mode.
\ -------------------------------------------------------------------

value %%gsrc

: GFastC ( x y fg bg c -- )
  $7F and                       \ x y fg bg c&$7F
  3 LSHIFT                      \ x y fg bg c<<3
  SystemFont[] +                \ x y fg bg SystemFont[c<<3]
  to %%gsrc                     \ x y fg bg
  8 0 do                        \ x y fg bg
    %%gsrc C@ 1 +to %%gsrc      \ x y fg bg n
    8 0 do                      \ x y fg bg n
      4 pick i +                \ x y fg bg n x+i
      4 pick j +                \ x y fg bg n x+i y+j
      2 pick $80 and if         \ x y fg bg n x+i y+j
        4 pick                  \ x y fg bg n x+i y+j fg
        1                       \ x y fg bg n x+i y+j fg TRUE
      else                      \ x y fg bg n x+i y+j
        3 pick                  \ x y fg bg n x+i y+j bg
        dup -1 = if             \ x y fg bg n x+i y+j bg
          0                     \ x y fg bg n x+i y+j bg FALSE
        else                    \ x y fg bg n x+i y+j bg
          1                     \ x y fg bg n x+i y+j bg TRUE
        then                    \ x y fg bg n x+i y+j bg flag
      then                      \ x y fg bg n x+i y+j c flag
      if                        \ x y fg bg n x+i y+j c
        GPutPixel               \ x y fg bg n
      else                      \ x y fg bg n x+i y+j c
        drop 2drop              \ x y fg bg n -- skips the pixel
      then                      \ x y fg bg n    if bg was -1
      1 LSHIFT                  \ x y fg bg n<<1
    loop                        \ x y fg bg n<<1
    drop                        \ x y fg bg
  loop                          \ x y fg bg
  2drop                         \ x y
  2drop                         \ --
;

\ ---[ FastS ]-------------------------------------------------------
\ Used to display a string to the display.
\
\ X Coordinate range : [0..39]
\ Y Coordinate range : [0..24]
\ FG/BG Range        : [0..255]
\
\ The x/y coordinate range for GFastS is 0..39 by 0..24, emulating
\ a 40 column text screen.  The x/y coordinates that it sends to the
\ GFastC word are scaled by multplying them by 8.
\ -------------------------------------------------------------------

: GFastS ( x y fg bg &str len -- )
  0 do                          \ x y fg bg %str
    4 pick i +                  \ x y fg bg %str x+i
    3 LSHIFT                    \ x y fg bg %str (x+i)*8
    4 pick                      \ x y fg bg %str x y
    3 LSHIFT                    \ x y fg bg %str x y*8
    4 pick                      \ x y fg bg %str x y fg
    4 pick                      \ x y fg bg %str x y fg bg
    4 pick i + C@               \ x y fg bg %str x y fg bg c
    GFastC                      \ x y fg bg %str
  loop                          \ x y fg bg %str
  2drop                         \ x y fg
  2drop                         \ x
  drop                          \ --
;

create %FillChar 80 C, here 80 dup allot BL fill

\ ---[ ClearText ]---------------------------------------------------
\ ClearText clears a box on the text screen, filling it with spaces
\ with the specified background color. The %FillChar array is used as
\ the source string, and it's base address and the number of chars to
\ be displayed are placed on the stack during the loop.
\ Note that if you specify a BG color of -1, then the box will not be
\ cleared, as the Space character has no pixels in it.

: GClearText ( x1 y1 x2 y2 fg bg -- )
  \ if x2>39 then make x2=39
  3 pick TextWidth 1- > if      \ x1 y1 x2 y2 fg bg
    >R rot                      \ x1 y1 y2 fg x2
    drop 39                     \ x1 y1 y2 fg 39
    -rot R>                     \ x1 y1 39 y2 fg bg
  then                          \ x1 y1 x2 y2 fg bg
  >R >R                         \ x1 y1 x2 y2
  1+                            \ x1 y1 x2 y2+1
  rot                           \ x1 x2 y2+1 y1
  R> -rot                       \ x1 x2 fg y2+1 y1
  R> -rot                       \ x1 x2 fg bg y2+1 y1
  do                            \ x1 x2 fg bg
    3 pick                      \ x1 x2 fg bg x1
    i                           \ x1 x2 fg bg x1 i
    3 pick                      \ x1 x2 fg bg x1 i fg
    3 pick                      \ x1 x2 fg bg x1 i fg bg
    %FillChar 1+                \ x1 x2 fg bg x1 i fg bg &fc[1]
    7 pick                      \ x1 x2 fg bg x1 i fg bg &fc[1] x2
    5 pick                      \ x1 x2 fg bg x1 i fg bg &fc[1] x2 x1
    -                           \ x1 x2 fg bg x1 i fg bg &fc[1] x2-x1
    1+                          \ x1 x2 fg bg x1 i fg bg &fc[1] len
    GFastS                      \ x1 x2 fg bg
  loop                          \ x1 x2 fg bg
  2drop                         \ x2 x2
  2drop                         \ --
;

\ ---[ ClearLine ]---------------------------------------------------
\ Clears a line on the screen to spaces, using the specified color
\ for the background
\ Note that if you specify a BG color of -1, then the line will not
\ be cleared, as the Space character has no pixels in it.

: GClearLine ( y f b -- )
  >R                            \ y fg
  0 -rot                        \ 0 y fg
  R>                            \ 0 y fg bg
  %FillChar 1+                  \ 0 y fg bg &fc[1]
  TextWidth                     \ 0 y fg bg &fc[1] 40
  GFastS                        \ --
;

: GWriteAT ( x y fg bg &str len -- )
  GFastS
;

: GWriteCenter ( y fg bg &str len -- )
  \ truncate if more than 40 characters in string
  dup TextWidth > if
    drop TextWidth
  then

  TextWidth 2/                  \ y fg bg &str len 20
  over                          \ y fg bg &str len 20 len
  2 /                           \ y fg bg &str len 20 len/2
  -                             \ y fg bg &str len 20-len/2
  -rot                          \ y fg bg x &str len
  >R >R                         \ y fg bg x
  swap                          \ y fg x bg
  >R                            \ y fg x
  -rot                          \ x y fg
  R> R> R>                      \ x y fg bg &str len
  GFastS                        \ --
;

\ ---[ Initialization of the module ]--------------------------------
\ Since this data may not be present on all computers, you should
\ leave this commented out until you try it on your computer first.

\ LoadBIOSFontData

\ ---[ A short demo ]------------------------------------------------

value %%c

: ShowCharset
  $13 InitGraph
  0 1 39 23 White Blue GClearText
  0 Yellow Black GClearLine
  0 2 Yellow -1 " 0123456789ABCDEF" GWriteAT
  8 0 do
   16 0 do
     i 8 * j 8 * 24 + White Cyan j 16 * i + GFastC
   loop
  loop
  17  3 Yellow -1 " 00..0Fh" GWriteAT
  17  4 Yellow -1 " 10..1Fh" GWriteAT
  17  5 Yellow -1 " 20..2Fh" GWriteAT
  17  6 Yellow -1 " 30..3Fh" GWriteAT
  17  7 Yellow -1 " 40..4Fh" GWriteAT
  17  8 Yellow -1 " 50..5Fh" GWriteAT
  17  9 Yellow -1 " 60..6Fh" GWriteAT
  17 10 Yellow -1 " 70..7Fh" GWriteAT

  0 13 White -1 " ---[Note]-----------------------------" GWriteAt
  0 14 White -1 " These are the available characters in" GWriteAT
  0 15 White -1 " the BIOS character font set located at" GWriteAT
  0 16 White -1 " F000h:FA6Eh on your computer." GWriteAT
  0 18 LightBlue -1 " Not too pretty, but you can make your" GWriteAt
  0 19 LightGray -1 " own sets and use them instead." GWriteAt
  0 20 White -1 " -------------------------[End Note]---" GWriteAt
  24 Yellow Black GClearLine
  0 to %%c
  begin
    key? not
  while
    25 3 38 10 White %%c GClearText
    0  %%c 16 + 32 mod  -1 " Graphics of the Second Kind Part V" GWriteCenter
    24 64 %%c - -1 " Press the ANYKEY" GWriteCenter
    10 0 do
      WaitRetrace     \ a delay to let the color changes be visible
    loop
    1 +to %%c
    %%c 64 mod to %%c
  repeat
  key drop
  CloseGraph
;

cr .( GotSK5: ShowCharset ) cr

