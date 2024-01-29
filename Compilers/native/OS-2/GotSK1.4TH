\ ---[ Code Addendum 05 ]-----------------------------[12/10/2006]---
\
\          For the Graphics of the Second Kind Part I column
\
\                         by Timothy Trussell
\
\ -------------------------------------------------------------------
\ This code is meant for use with the 32Forth system.
\ This is the DOS DPMI version of the compiler in Rick van Norman's
\ OS2FORTH.ZIP package, available on the Taygeta Scientific site.
\ -------------------------------------------------------------------

\ ---[ PCX Library ]-------------------------------------------------
\ This version of the PCX image library implements the Mini-OOF code
\ for the structure instance, and a level of inheritance capability.
\ -------------------------------------------------------------------

exists [GotSK1] not [if]       \ skip if already loaded

: [GotSK1] ;

\ ---[ Object Oriented Forth - Bernd Paysan ]------------------------
\ Mini-OOF                                                 12apr98py
: method ( m v -- m' v ) Create  over , swap cell+ swap
  DOES> ( ... o -- ... ) @ over @ + @ execute ;
: var ( m v size -- m v' ) Create  over , +
  DOES> ( o -- addr ) @ + ;
: class ( class -- class methods vars ) dup 2@ ;
: end-class  ( class methods vars -- )
  Create  here >r , dup , 2 cells ?DO ['] noop , 1 cells +LOOP
  cell+ dup cell+ r> rot @ 2 cells /string move ;
: defines ( xt class -- ) ' >body @ + ! ;
: new ( class -- o )  here over @ allot swap over ! ;
: :: ( class "name" -- ) ' >body @ + @ compile, ;
Create object  1 cells , 2 cells ,

\ ---[ PCX Class ]---------------------------------------------------
\ Define an object that will contain the header, palette, and program
\ specific data for the PCX file being loaded.
\ Note that the actual compressed image data is not transferred to
\ this object, although the 128 byte header and the 768 byte palette
\ data blocks are.

object class
\ This data is present in the PCX file itself
   1 var .manufact
   1 var .version
   1 var .encoding
   1 var .bitsperpixel
   2 var .xmin
   2 var .ymin
   2 var .xmax
   2 var .ymax
   2 var .hdpi
   2 var .vdpi
  48 var .palette16
   1 var .reserved
   1 var .colorplanes
   2 var .bytesperline
   2 var .palinfo
  58 var .filler1

\ This data is the module info for each PCX loaded
  16 var .fname                 \ filename to use
cell var .fsize                 \ actual # of bytes in file
cell var .data                  \ ptr to data buf of raw RLE image
cell var .expanded              \ ptr to 64k destination buffer
 768 var .palette               \ palette buffer
cell var .width                 \ image width - dpi
cell var .height                \ image height - dpi
cell var .isize                 \ width*height
cell var .filler2               \ to round off size of structure

  method (SetName)              \ sets the name in struct
  method (SetBuffer)            \ sets address of buffer in struct
  method LoadImage              \ loads an image data
  method LoadPalette            \ loads the palette data
  method SetImagePalette        \ sends palette to VGA card
  method ImageToBuffer          \ decompress the image to a buffer
  method ImageToScreen          \ draws buffer image to VGA memory
end-class pcx

\ ---[ Method Prototypes ]-------------------------------------------
\ (SetName)             ( &str len &obj -- )
\ (SetBuffer)           ( &buffer &obj -- )
\ LoadImage             ( &str len &dst size &obj -- )
\ LoadPalette           ( &obj -- )
\ SetImagePalette       ( &obj -- )
\ ImageToBuffer         ( &buf &obj -- )
\ ImageToScreen         ( &buf x y &obj -- )

\ ---[ Ancilliary Words ]--------------------------------------------
\ I need some glue to allow the Methods to be implemented.

code CPortOut ( c addr -- )
                                \ addr in bx on entry
                 bx dx mov      \ addr to dx
                    ax pop      \ c to ax (al, specifically)
                       $EE C,   \ OUT DX,AL
                    bx pop      \ get new TOS
                    end-code
                    NO-EXPAND

value PSelector $0A000 SEG>DESC to PSelector

\ ---[ VPlotLine ]---------------------------------------------------
\ A quick and dirty word for putting a line of pixels from an image
\ to the VGA screen.
\ Meant for single planar (320x200x256) mode at this point.
\ Will have to be modified for ModeX and VESA access.
\ This does not clip the image at the screen borders.

code VPlotLine ( &buf len x y -- )
                    bx  push
                 bp sp  xchg
                    es  push
                    di  push
                    si  push
        PSelector # ax  mov
                 ax es  mov     \ es=VSelector
       1 cells [bp] di  mov     \ di=x
       0 cells [bp] ax  mov     \ ax=y
                6 # cl  mov
                 ax cl  shl     \ ax=y*64
                 ax di  add     \ di=y*64+x
                2 # cl  mov
                 ax cl  shl     \ ax=y*256
                 ax di  add     \ di=y*320+x
                        cld
       3 cells [bp] si  mov     \ si=&buf
       2 cells [bp] cx  mov     \ cx=len
                        rep
                    al  movsb
                    si  pop     \ restore registers
                    di  pop
                    es  pop
                 bp sp  xchg    \ restore stack frame
          4 cells # sp  add     \ drop parameters
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

\ ---[ Variables ]---------------------------------------------------

0 constant ByteMode
1 constant RunMode
value %bmode
value %srcptr
value %dstptr
value %endptr
value %bytecount
value %outbyte

\ ---[ (SetName) Method ]--------------------------------------------
\ Internal use

:noname ( &str len &obj -- )
  over                          \ &str len &obj len
  >R                            \ &str len &obj
  .fname                        \ &str len &obj.fname
  R>                            \ &str len &obj.fname len
  over                          \ &str len &obj.fname len &obj.fname
  C!                            \ &str len &obj.fname
  1+                            \ &str len &obj.fname+1
  swap                          \ &str &obj.fname+1 len
  cmove                         \ --
; pcx defines (SetName)

\ ---[ (SetBuffer) Method ]------------------------------------------
\ Internal use

:noname ( &buffer &obj -- )
  .expanded !
; pcx defines (SetBuffer)

\ ---[ LoadPalette Method ]------------------------------------------
\ copies the palette data to the &obj.palette array
\ The palette data is the last 768 bytes in the PCX file, which is
\ why we stored how many bytes were actually read in .fsize
\ The PCX palette data is multiplied by 4 when it is stored in the
\ image file, so we have to divide by 4 to be able to use it.

:noname ( &obj -- )
  \ add check to drop to text mode on error
  dup .isize @ 0= if            \ &obj
    drop TRUE abort" LoadPalette: No file loaded"
  then                          \ &obj
  dup                           \ &obj &obj
  .data @                       \ &obj &obj.data
  over                          \ &obj &obj.data &obj
  .fsize @                      \ &obj &obj.data fsize
  +                             \ &obj EndOfData
  768 -                         \ &obj EndOfData-768
  768 0 do                      \ &obj &RawPaletteData
    dup                         \ &obj &RP &RP
    i +                         \ &obj &RP &RP[i]
    C@                          \ &obj &RP c
    2 RSHIFT                    \ &obj &RP c>>2
    2 pick                      \ &obj &FP c>>2 &obj
    .palette                    \ &obj &FP c>>2 &obj.palette
    i +                         \ &obj &FP c>>2 &obj.palette[i]
    C!                          \ &obj &RP
  loop                          \ &obj &RP
  2drop
; pcx defines LoadPalette

\ ---[ SetImagePalette Method ]--------------------------------------

:noname ( &obj -- )
 0 $03C8 CPortOut
 768 0 do
   dup .palette i + C@ $03C9 CPortOut
 loop
 drop
; pcx defines SetImagePalette

\ ---[ LoadImage Method ]--------------------------------------------
\ Loads the .PCX file into main system memory.
\ Copies the 128 byte header to the object structure
\ Does NOT process the palette or image data

:noname ( &str len &dst size &obj -- 0/-1 )
  >R                            \ &str len &dst size
  R@ .fsize !                   \ &str len &dst           save size
  R@ .data !                    \ &str len                set dst ptr
  R@ (SetName)                  \ --                      set name
\ perform the "using" word function from 32Forth
  R@ .fname count               \ &str len
  r/w open-file                 \ hndl 0/1
  if                            \                     abort if IOR !0
    \ want to drop back to text mode if error
    cr ." LoadImage: " R> .fname count type space
    TRUE abort" not found"
  then
  block-fid !                   \                    else save handle
\ load the data to the target array from the disc
  R@ .data @ R@ .fsize @ block-fid @ read-file
\ drop the IOR, and save the actual # of bytes read
  drop R@ .fsize !
\ close the file
  block-fid @ close-file drop   \                        lose the ior
\ copy the 128-byte header to the &obj structure
  R@ .data @ R@ .manufact 128 cmove
\ calculate the width and height data
  R@ .xmax H@                   \ xmax
  R@ .xmin H@ - 1+              \ xmax-xmin+1
  dup R@ .width !               \ width
  R@ .ymax H@                   \ width ymax
  R@ .ymin H@ - 1+              \ width ymax-ymin+1
  dup R@ .height !              \ width height
  * R> .isize !                 \ --
; pcx defines LoadImage

\ ---[ ImageToBuffer Method ]----------------------------------------
\ Decompress the encoded image to the specified buffer array.
\ Requires the destination buffer address as a parameter
\ The data is stored as a linear array, so when accessed you have to
\ know the width of each line - or else it's gonna look REAL bad.

:noname ( &buf &obj -- )
  dup .isize @ 0= if            \ &obj
    2drop TRUE abort" ImageToBuffer: No file loaded"
  then                          \ &obj
  >R
  R@ .expanded !
  ByteMode to %bmode
  R@ .data @ 128 + to %srcptr   \       offset to start of image data
  R@ .expanded @ to %dstptr     \           where to store pixel data
  R@ .data @
  R> .fsize @ +
  768 -
  to %endptr       \ so we end at the end of the data stream at least
  begin
    %srcptr %endptr <
  while
    %bmode 0= if                \
      %srcptr C@ to %outbyte    \
      1 +to %srcptr             \
      %outbyte $0BF > if        \          if >= $C0, multiple pixels
        %outbyte $03F and
        to %bytecount           \                      get byte count
        %srcptr C@ to %outbyte  \                   get pixel to plot
        1 +to %srcptr           \
        -1 +to %bytecount
        %bytecount 0> if
          RunMode to %bmode
        then
      then
    else
      -1 +to %bytecount
      %bytecount 0= if
        ByteMode to %bmode
      then
    then
    %outbyte %dstptr C!
    1 +to %dstptr
  repeat
; pcx defines ImageToBuffer

\ ---[ ImageToScreen Method ]----------------------------------------
\ Draws the decompressed image to the VGA screen memory.
\ Graphics mode needs to have been enabled already
\ The palette will need to be set to view the image correctly, but
\ my test program toggles back and forth so you can see the image
\ with the default palette, and then with the palette from the PCX
\ file setup.
\ This does not clip the image at the borders, so be careful.

:noname ( &buf x y &obj -- )
  dup .isize @ 0= if            \ &obj
    2drop 2drop TRUE abort" ImageToScreen: No file loaded"
  then                          \ &obj
  dup .height @ 0 do             \ &buf x y &obj
    3 pick                       \ &buf x y &obj &buf
    over .width @                \ &buf x y &obj &buf w
    i *                          \ &buf x y &obj &buf w*i
    +                            \ &buf x y &obj &buf[w*i]
    over .width @                \ &buf x y &obj &buf[w*i] len
    4 pick                       \ &buf x y &obj &buf[w*i] len x
    4 pick i +                   \ &buf x y &obj &buf[w*i] len x y+i
    VPlotLine                    \ &buf x y &obj
  loop                           \ &buf x y &obj
  2drop                          \ &buf x
  2drop                          \ -
; pcx defines ImageToScreen

\ ---[ End of PCX Library ]------------------------------------------

\ --[ Testing Words ]------------------------------------------------

code PSetMode ( mode -- )
                 bx ax  mov
              int10 #)  call
                    bx  pop
                    end-code
                    NO-EXPAND

: InitGraph ( mode -- ) PSetMode ;
: CloseGraph ( -- )   3 PSetMode ;

\ ---[ BufToScreen ]-------------------------------------------------
\ Copies the specified buffer to the video screen at offset 0
\ Buffer size is expected to be 64000 bytes.

code BufToScreen ( &src -- )
                    es  push
                    di  push
                    si  push
        PSelector # ax  mov
                 ax es  mov
                 di di  xor
                 bx si  mov
            16000 # cx  mov
                        cld
                   rep  movs
                    si  pop
                    di  pop
                    es  pop
                    bx  pop     \ get new TOS
                    end-code
                    no-expand

\ Allocate some virtual video buffers
create VPage0[] here 65536 dup allot 0 fill
create VPage1[] here 65536 dup allot 0 fill

\ Load the image file data here
create Image1[] here 65536 dup allot 0 fill

\ Declare an instance of the class structure pcx
pcx new constant Image1

: Load1
  " bots.pcx" Image1[] 65536 Image1 LoadImage
  Image1 LoadPalette
  VPage1[] Image1 ImageToBuffer
;

\ ---[ Demo Words ]--------------------------------------------------
\ These three words do the same thing - they display the image file
\ that you loaded with the Load1 word above.  But, they do it in a
\ different way in each word.  Pressing the ESC key (sometimes twice)
\ will exit each program.  The first part shows the image with the
\ default palette in the video card.  When you press a key, the
\ palette from the image file is sent to the video display, and the
\ PCX image will (should) be displayed correctly.  It will cycle like
\ this until you press the ESC key to exit.

\ ---[ Show1 ]-------------------------------------------------------
\ This draws the PCX image to a buffer, which is then blitted to the
\ VGA screen using the BufToScreen word.

: Show1
  Load1
  Image1 .height @ 0 do                 \ "arrange" the data, so that
    VPage1[] i Image1 .width @ * +      \ they start in the right ofs
    VPage0[] i 320 * +                  \ in the buffer before it is
    Image1 .width @                     \ sent to the video screen.
    cmove
  loop
  0
  begin
    $13 InitGraph
    VPage0[] BufToScreen
    key 27 = if drop 1 then
    Image1 SetImagePalette
    key 27 = if drop 1 then
    dup
  until
  drop
  CloseGraph
;

\ ---[ Show2 ]-------------------------------------------------------
\ This draws the PCX image directly to the VGA screen using the
\ VPlotLine call, which plots a single line of the image
\ VPlotLine allows you to tell it the x/y coordinate of the upper
\ left corner of the image on the screen.

: Show2
  Load1
  0
  begin
    $13 InitGraph
    Image1 .height @ 0 do
      VPage1[] i Image1 .width @ * +
      Image1 .width @
      320 Image1 .width @ - 2/
      200 Image1 .height @ - 2/ i +
      VPlotLine                   \ center on screen
    loop
    key 27 = if drop 1 then
    Image1 SetImagePalette
    key 27 = if drop 1 then
    dup
  until
  drop
  CloseGraph
;

\ ---[ Show3 ]-------------------------------------------------------
\ This version uses the display implementation I have coded into the
\ pcx class with the method [ ImageToScreen ]

: Show3
  Load1
  0
  begin
    $13 InitGraph
    VPage1[]
    320 Image1 .width @ - 2/     \ center the image on the screen
    200 Image1 .height @ - 2/
    Image1 ImageToScreen
    key 27 = if drop 1 then
    Image1 SetImagePalette
    key 27 = if drop 1 then
    dup
  until
  drop
  CloseGraph
;

[then]

cr .( GotSK1: Show1 Show2 and Show3 are the demos)

