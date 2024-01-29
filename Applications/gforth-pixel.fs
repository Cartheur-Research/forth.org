0 [IF] ==============================================================
     File Name: gforth-pixel.fs
       Version: 1.00
        Author: Timothy Trussell
          Date: 06/20/2011
   Description: gforth SDL Interface - PutPixel/GetPixel functions
  Forth System: gforth-0.7.0
  Linux System: Ubuntu v10.04 i386, kernel 2.6.32-32
  C++ Compiler: gcc version 4.4.3 (Ubuntu 4.4.3-4ubuntu5)
=====================================================================
While this module has been coded as stand-alone, it is actually an
extract from my updated SDL Library for gforth.
 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
This libcc is relevant to plotting pixels/lines to SDL surfaces.
============================================================== [THEN]

c-library gforth_pixel_lib

s" SDL" add-lib

\c #include <SDL/SDL.h>
\c #include <stdlib.h>
\c #include <unistd.h>
\c #include <math.h>
\c #include <stdio.h>

\ ============[ F U N C T I O N   D E F I N I T I O N S ]============

\ ---[ General PutPixel function ]---

\c void ppixel(SDL_Surface *dst, int x, int y, Uint32 pixel)
\c {
\c   int bpp = dst->format->BytesPerPixel;
\c   /* Here p is the address to the pixel we want to set */
\c   Uint8 *p = (Uint8 *)dst->pixels + y * dst->pitch + x * bpp;
\c   switch(bpp) {
\c   case 1:
\c      *p = pixel;
\c      break;
\c   case 2:
\c      *(Uint16 *)p = pixel;
\c      break;
\c   case 3:
\c      if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
\c        p[0] = (pixel >> 16) & 0xff;
\c        p[1] = (pixel >> 8) & 0xff;
\c        p[2] = pixel & 0xff;
\c      } else {
\c        p[0] = pixel & 0xff;
\c        p[1] = (pixel >> 8) & 0xff;
\c        p[2] = (pixel >> 16) & 0xff;
\c      }
\c      break;
\c   case 4:
\c      *(Uint32 *)p = pixel;
\c      break;
\c   }
\c }

\ ---[ Fast 8-bpp PutPixel function ]---

\c void ppixel8( SDL_Surface *dst, int x, int y, Uint32 pixel )
\c {
\c   int bpp = dst->format->BytesPerPixel;
\c   /* Convert the pixels to 8 bit */
\c   Uint8 *p = (Uint8 *)dst->pixels + y * dst->pitch + x * bpp;
\c   /* Set the pixel */
\c   *p = pixel;
\c }

\ ---[ Fast 16-bpp PutPixel function ]---

\c void ppixel16( SDL_Surface *dst, int x, int y, Uint32 pixel )
\c {
\c   int bpp = dst->format->BytesPerPixel;
\c   /* Convert the pixels to 16 bit */
\c   Uint16 *p = (Uint16 *)dst->pixels + y * dst->pitch + x * bpp;
\c   /* Set the pixel */
\c   *p = pixel;
\c }

\ ---[ Fast 24-bpp PutPixel function ]---

\c void ppixel24( SDL_Surface *dst, int x, int y, Uint32 pixel )
\c {
\c   int bpp = dst->format->BytesPerPixel;
\c   /* Convert the pixels to 24 bit */
\c   Uint8 *p = (Uint8 *)dst->pixels + y * dst->pitch + x * bpp;
\c   /* Set the pixel */
\c   if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
\c     p[0] = (pixel >> 16) & 0xff;
\c     p[1] = (pixel >> 8) & 0xff;
\c     p[2] = pixel & 0xff;
\c   } else {
\c     p[0] = pixel & 0xff;
\c     p[1] = (pixel >> 8) & 0xff;
\c     p[2] = (pixel >> 16) & 0xff;
\c   }
\c }

\ ---[ Fast 32-bpp PutPixel function ]---

\c void ppixel32( SDL_Surface *dst, int x, int y, Uint32 pixel )
\c {
\c   int bpp = dst->format->BytesPerPixel;
\c   /* Convert the pixels to 32 bit */
\c   Uint32 *p = (Uint32 *)dst->pixels + y * dst->pitch + x * bpp;
\c   /* Set the pixel */
\c   *p = pixel;
\c }

\ ---[ General GetPixel function ]---

\c Uint32 gpixel(SDL_Surface *src, int x, int y)
\c {
\c   int bpp = src->format->BytesPerPixel;
\c   /* Here p is the address to the pixel we want to retrieve */
\c   Uint8 *p = (Uint8 *)src->pixels + y * src->pitch + x * bpp;
\c   switch(bpp) {
\c     case 1:
\c       return *p;
\c     case 2:
\c       return *(Uint16 *)p;
\c     case 3:
\c       if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
\c         return p[0] << 16 | p[1] << 8 | p[2];
\c       else
\c         return p[0] | p[1] << 8 | p[2] << 16;
\c     case 4:
\c       return *(Uint32 *)p;
\c     default:
\c       return 0; /* shouldn't happen, but avoids warnings */
\c   }
\c }

\ ---[ Fast 8-bpp GetPixel function ]---

\c Uint32 gpixel8( SDL_Surface *src, int x, int y )
\c {
\c   int bpp = src->format->BytesPerPixel;
\c   Uint8 *p = (Uint8 *)src->pixels + y * src->pitch + x * bpp;
\c   /* Get the requested pixel */
\c   return *p;
\c }

\ ---[ Fast 16-bpp GetPixel function ]---

\c Uint32 gpixel16( SDL_Surface *src, int x, int y )
\c {
\c   int bpp = src->format->BytesPerPixel;
\c   Uint16 *p = (Uint16 *)src->pixels + y * src->pitch + x * bpp;
\c   /* Get the requested pixel */
\c   return *p;
\c }

\ ---[ Fast 24-bpp GetPixel function ]---

\c Uint32 gpixel24( SDL_Surface *src, int x, int y )
\c {
\c   int bpp = src->format->BytesPerPixel;
\c   Uint8 *p = (Uint8 *)src->pixels + y * src->pitch + x * bpp;
\c   /* Get the requested pixel */
\c   if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
\c     return p[0] << 16 | p[1] << 8 | p[2];
\c   else
\c     return p[0] | p[1] << 8 | p[2] << 16;
\c }

\ ---[ Fast 32-bpp GetPixel function ]---

\c Uint32 gpixel32( SDL_Surface *src, int x, int y )
\c {
\c   int bpp = src->format->BytesPerPixel;
\c   Uint32 *p = (Uint32 *)src->pixels + y * src->pitch + x * bpp;
\c   /* Get the requested pixel */
\c   return *p;
\c }

\ =================[ L I B C C   I N T E R F A C E ]=================

c-function SDL_PutPixel                 ppixel        a n n n -- void
c-function SDL_PutPixel8                ppixel8       a n n n -- void
c-function SDL_PutPixel16               ppixel16      a n n n -- void
c-function SDL_PutPixel24               ppixel24      a n n n -- void
c-function SDL_PutPixel32               ppixel32      a n n n -- void

c-function SDL_GetPixel                 gpixel             a n n -- n
c-function SDL_GetPixel8                gpixel8            a n n -- n
c-function SDL_GetPixel16               gpixel16           a n n -- n
c-function SDL_GetPixel24               gpixel24           a n n -- n
c-function SDL_GetPixel32               gpixel32           a n n -- n

\ These are added for the demo code following this libcc interface

c-function SDL_Init                     SDL_Init               n -- n
c-function SDL_Flip                     SDL_Flip               a -- n
c-function SDL_GetError                 SDL_GetError             -- a
c-function SDL_LockSurface              SDL_LockSurface        a -- n
c-function SDL_MapRGB                   SDL_MapRGB       a n n n -- n
c-function SDL_Pollevent                SDL_PollEvent          a -- n
c-function SDL_Quit                     SDL_Quit              -- void
c-function SDL_SetVideoMode             SDL_SetVideoMode n n n n -- a
c-function SDL_UnlockSurface            SDL_UnlockSurface   a -- void

end-c-library

\ ---[ Define the aligned field sizes ]---

[IFUNDEF] cell%   4 4 2CONSTANT cell%  [ENDIF]
[IFUNDEF] ptr%    4 4 2CONSTANT ptr%   [ENDIF]
[IFUNDEF] int%    4 4 2CONSTANT int%   [ENDIF]
[IFUNDEF] word%   2 2 2CONSTANT word%  [ENDIF]
[IFUNDEF] byte%   1 1 2CONSTANT byte%  [ENDIF]
[IFUNDEF] zptr%   1 0 2CONSTANT zptr%  [ENDIF]

[IFUNDEF] =: : =: CONSTANT ; [ENDIF]

\ The required data structures for this module

struct
  word% field SDL_Offset->x
  word% field SDL_Offset->y
  word% field SDL_Offset->w
  word% field SDL_Offset->h
end-struct SDL_Rect%

struct
  int%      field SDL_Surface->flags
  ptr%      field SDL_Surface->format
  int%      field SDL_Surface->w
  int%      field SDL_Surface->h
  word%     field SDL_Surface->pitch
  ptr%      field SDL_Surface->pixels
  int%      field SDL_Surface->offset
  ptr%      field SDL_Surface->hwdata
  SDL_Rect% field SDL_Surface->cliprect
  int%      field SDL_Surface->unused1
  int%      field SDL_Surface->locked
  ptr%      field SDL_Surface->map
  int%      field SDL_Surface->formatversion
  int%      field SDL_Surface->refcount
end-struct SDL_Surface%

\ ---[ SDL_MustLock ]------------------------------------------------
\ This is coded as a macro - not as an actual C++ function.

$00000001       =: SDL_HWSURFACE
$00000004       =: SDL_ASYNCBLIT
$00004000       =: SDL_RLEACCEL

: SDL_MustLock { *src -- f }
  0 SDL_HWSURFACE OR SDL_ASYNCBLIT OR SDL_RLEACCEL OR 
  *src SDL_Surface->flags @ AND
  *src SDL_Surface->offset @ AND
;

\ ==========[ G F O R T H   P I X E L   F U N C T I O N S ]==========
\ The high level wrapper functions provide for Locking and Unlocking
\ the SDL surface being plotted to.

: PutPixel { *dst _x _y _pixel -- }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y _pixel SDL_PutPixel
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: PutPixel8 { *dst _x _y _pixel -- }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y _pixel SDL_PutPixel8
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: PutPixel16 { *dst _x _y _pixel -- }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y _pixel SDL_PutPixel16
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: PutPixel24 { *dst _x _y _pixel -- }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y _pixel SDL_PutPixel24
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: PutPixel32 { *dst _x _y _pixel -- }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y _pixel SDL_PutPixel32
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: GetPixel { *dst _x _y -- pixel }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y SDL_GetPixel
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: GetPixel8 { *dst _x _y -- pixel }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y SDL_GetPixel8
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: GetPixel16 { *dst _x _y -- pixel }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y SDL_GetPixel16
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: GetPixel24 { *dst _x _y -- pixel }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y SDL_GetPixel24
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

: GetPixel32 { *dst _x _y -- pixel }
  *dst SDL_MustLock if *dst SDL_LockSurface drop then  \ lock surface
  *dst _x _y SDL_GetPixel32
  *dst SDL_MustLock if *dst SDL_UnlockSurface then   \ unlock surface
;

1 [IF]

\ ==================[ C O D I N G   E X A M P L E ]==================
\ In the above definitions, each of the Put/GetPixel functions call
\ the SDL surface lock/unlock functions for each and every pixel that
\ they plot.  This is fine if your program code is only drawing a
\ small number of pixels.  However, if a large number of pixels are
\ being drawn, it is more efficient to Lock the surface once, plot
\ all of the pixels, and then Unlock the surface.
\
\ In this case, calling the SDL_PutPixel and SDL_GetPixel functions
\ directly should be used, not the corresponding PutPixel/GetPixel
\ functions.
\
\ An example of this is the Line functions below.
\ --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

\ ---[ Line-NoLock ]-------------------------------------------------
\ Draws a Bresenham line from x1,y1 to x2,y2 in the color pixel
\ Does not lock/unlock the surface.

: Line-NoLock ( *dst x1 y1 x2 y2 pixel -- )
  0 0 0 0 0 0 0 0 0                             \ add local variables
  { *dst _x1 _y1 _x2 _y2 _pixel _d _x _y _ax _ay _sx _sy _dx _dy -- }

  _x2 _x1 - dup dup to _dx abs 2* to _ax 0< if -1 else 1 then to _sx
  _y2 _y1 - dup dup to _dy abs 2* to _ay 0< if -1 else 1 then to _sy

  _x1 to _x
  _y1 to _y

  _ax _ay > if
    _ay _ax 2/ - to _d
    begin
      _x _x2 = 0=                                    \ while _x != x2
    while
      *dst _x _y _pixel SDL_PutPixel
      _d 0 >= if
        _sy _y + to _y
        _ax negate _d + to _d
      then
      _sx _x + to _x
      _ay _d + to _d
    repeat
  else                                                  \ ax not > ay
    _ax _ay 2/ - to _d
    begin
      _y _y2 = 0=                                    \ while _y != y2
    while
      *dst _x _y _pixel SDL_PutPixel
      _d 0 >= if
        _sx _x + to _x
        _ay negate _d + to _d
      then
      _sy _y + to _y
      _ax _d + to _d
    repeat
  then
;

\ ---[ Line ]--------------------------------------------------------
\ Draws a Bresenham line from x1,y1 to x2,y2 in the color pixel.
\ Locks and Unlocks the surface for each pixel plotted.

: Line ( *dst x1 y1 x2 y2 pixel -- )
  5 pick >R
  R@ SDL_MustLock if R@ SDL_LockSurface drop then      \ lock surface
  Line-NoLock                                         \ draw the line
  R@ SDL_MustLock if R@ SDL_UnlockSurface then       \ unlock surface
  R> drop
;

\ ========[ D E M O   T H A T   E X E R C I S E S   L I N E ]========

$0000FFFF       =: SDL_INIT_EVERYTHING
2               =: SDL_KEYDOWN

struct
  byte% field SDL_Event->type
  1 0   field SDL_Event->payload
end-struct event-type%

event-type%
  1 32 field SDL_Event->data
end-struct SDL_Event%

   0 VALUE screen-surface                 \ pointer to screen surface
1024 VALUE screen-w                              \ screen width value
 768 VALUE screen-h                             \ screen height value
  32 VALUE screen-bpp                   \ bits per pixel of this mode
   0 VALUE in-graphics-mode

   0 VALUE #Black                                        \ SDL Colors
   0 VALUE #Blue
   0 VALUE #Green
   0 VALUE #Red
   0 VALUE #White

create keyevent here SDL_Event% nip dup allot 0 fill

: SDL-Keypressed ( -- boolean )
  FALSE
  begin
    keyevent SDL_PollEvent            \ are there any pending events?
  while
    keyevent SDL_Event->type c@             \ yes, process the events
    case
      SDL_KEYDOWN of drop TRUE endof         \ for ANY keypress event
    endcase
  repeat                      \ until no more events are in the queue
;

: SDL-NoWaitKey ( -- ) begin SDL-Keypressed until ;

: Set-Colors ( -- )
  screen-surface SDL_Surface->format @ >R
  R@    0    0    0 SDL_MapRGB to #Black
  R@    0    0 $0FF SDL_MapRGB to #Blue
  R@    0 $0FF    0 SDL_MapRGB to #Green
  R@ $0FF    0    0 SDL_MapRGB to #Red
  R> $0FF $0FF $0FF SDL_MapRGB to #White
;

: Error-End { _flag *str _len -- }
  _flag 0<> if              \ if flag is !=0, then there was an error
    in-graphics-mode if       \ turn off graphics mode for error exit
      SDL_Quit                                 \ exit the SDL systems
      0 to in-graphics-mode                         \ reset mode flag
    then
    *str _len type cr                       \ now display the message
    bye                             \ and exit gforth to the terminal
  then
;

: InitGraph ( -- )
  SDL_INIT_EVERYTHING SDL_Init          \ initialize SDL video system
  0<> s" Unable to initialize SDL" Error-End      \ check return code

  screen-w screen-h screen-bpp SDL_HWSURFACE SDL_SetVideoMode
  dup 0< s" Unable to set video mode" Error-End   \ check return code
  to screen-surface                  \ save pointer to screen surface

  Set-Colors                      \ Initialize the advanced color set
  1 to in-graphics-mode          \ set flag because graphics are 'on'
;

: CloseGraph ( -- )
  SDL_Quit                                            \ graceful exit
  0 to in-graphics-mode                           \ set flag to 'off'
;

\ Draw lots of lines

: Draw-Side ( *dst #side lock -- )
  0 0 { *dst #side _lock _n _n/5 -- }
  #side case
    0 of 5 to _n endof
    1 of 5 to _n endof
    2 of 5 to _n endof
    3 of 0 to _n endof
  endcase
  begin
    #side case
      0 of _n screen-w < endof
      1 of _n screen-h < endof
      2 of _n screen-h < endof
      3 of _n screen-w < endof
    endcase
  while
    _n 5 / to _n/5
    *dst screen-w 2/ screen-w 10 /
    #side case
      0 of - _n/5 + screen-h 5 / _n screen-h 1- #Blue endof
      1 of - _n/5 0 _n #Green endof
      2 of + _n/5 screen-w 1- _n #Red endof
      3 of - _n/5 + screen-h 1- _n 0 #White endof
    endcase
    _lock if Line else Line-NoLock then
    _n 10 + to _n
  repeat
;

\ LineDemo uses the Line function that locks/unlocks the surface

: LineDemo ( -- )
  InitGraph
  4 0 do
    screen-surface i TRUE Draw-Side
  loop
  screen-surface SDL_Flip drop                    \ Display the image
  SDL-NoWaitKey                                 \ Wait for a keypress
  CloseGraph
;

\ LineDemo2 uses the line function that does *not* lock the surface.
\ This is done prior/after the calls to plotting the lines.

: LineDemo2 ( -- )
  InitGraph
  screen-surface SDL_MustLock if
    screen-surface SDL_LockSurface drop            \ lock the surface
  then
  4 0 do
    screen-surface i FALSE Draw-Side
  loop
  screen-surface SDL_MustLock if
    screen-surface SDL_UnlockSurface             \ unlock the surface
  then
  screen-surface SDL_Flip drop                    \ Display the image
  SDL-NoWaitKey                                 \ Wait for a keypress
  CloseGraph
;

cr .( gforth Fast Pixel Demo: LineDemo, LineDemo2)
cr .( Press the <ANYKEY> to exit)
cr

[THEN]
