
Something for those of you also using the L.O.V.E. Forth system...

                       ----snip snip ----

\ FastS - L.O.V.E. FORTH Fast Screen Write module

\   The code was assembled using the A86 assembler only.

hex
variable ScreenSeg  B800 ScreenSeg !
decimal

code FastS                      ; x y attr str --
    extrn DATA@ScreenSeg:W

_WorkStr  = word ptr [bp+2]     ; where our parms are
_WorkAttr = word ptr [bp+4]
_WorkY    = word ptr [bp+6]
_WorkX    = word ptr [bp+8]

    push bp
    mov  bp,sp
    push si,ds,di,es            ; save registers

; Calculate offset

    mov  ax,_WorkY
    mov  bx,_WorkX
    mov  ch,al                  ; CH = Y

    xor  ax,ax                  ; AX = 0
    mov  cl,al                  ; CL = 0
    mov  bh,al                  ; BH = 0
    dec  ch                     ; Y (in CH) to 0..24 range
    shr  cx,1                   ; CX = Y * 128
    mov  di,cx                  ; Store in DI
    shr  di,1                   ; DI = Y * 64
    shr  di,1                   ; DI = Y * 32
    add  di,cx                  ; DI = (Y * 160)
    dec  bx                     ; X (in BX) to 0..79 range
    shl  bx,1                    Account for attribute bytes
    add  di,bx                  ; DI = (Y * 160) + (X * 2)

    mov  bx,_WorkStr            ; get length of string
    mov  al,byte ptr es:[bx]    ; AX was 0'd above
    mov  cx,ax                  ; ...into CX - max len of 255
    inc  bx                     ; set string address
    mov  si,bx                  ; and put it into SI

    mov  bx,_WorkAttr           ; get the attribute byte

    mov  ax,word ptr es:DATA@ScreenSeg
    mov  es,ax                  ; put video segment into ES
    mov  ah,bl                  ; put attribute into AH
    pop  ds                     ; get VS: into DS
    push ds
    cld                         ; SI/DI are incremented
L1:                             ; A86 local label
    lodsb                       ; get the string byte
    stosw                       ; save the char/attr to video
    loop  L1                    ; dec CX and loop until CX=0

    pop  es,di,ds,si            ; restore changed registers
    mov  sp,bp                  ; restore the stack frame
    pop  bp
    add  sp,8                   ; remove the parameters
    next                        ; execute the next 4th word...
end-code

: Attr 16 * + ;   ( fg bg -- attr )

                       ----snip snip ----

Save this to a file, such as FASTS.4th in standard ASCII format.

To load, if you have the A86 assembler configuration (.cfg)
file properly setup AND have it on your system, use the command

                  include" fasts.4th"

and the code will be compiled and linked.

Usage syntax is:  x y attr " string data" FastS
where:
               x,y  are coordinates on the screen
              attr  is a combined attribute byte
             " .."  is any valid FORTH string

Example:
             10 12 23 " My Name Is Tim" FastS

puts the string " .." at 10,23 in white text on a blue background.
If you don't want to figure out the combined attribute byte, just
use Attr, which takes parameters (fg,bg) and returns the combined
attribute byte.

