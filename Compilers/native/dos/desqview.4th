((
  This unit adds support for DESQview

  Last Revised:         9/17/90
  LOVE 4th Conversion: 11/06/93

  Coded for compilation with the A86 configuration.  Others will require
  minor code changes [the PUSH/POP opcodes, basically] to compile correctly.
))

variable DV_Loaded      \ True if running under DESQview
variable DV_Version     \ DESQview" version number

code DV_There                   ; -- 0/1
     extrn DATA@DV_Loaded:W
     extrn DATA@DV_Version:W

     push bp
     mov  bp,sp
     push ds,si,es,di
     mov  w es:DATA@DV_Version,0h
     mov  cx,4445h              ; "DE"
     mov  dx,5351h              ; "SQ"
     mov  ax,2B01h
     int  21h
     xor  cx,cx                 ; initial false value
     pop  di,es,si,ds,bp        ; restore registers
     cmp  al,0FFh               ; if DV not present, AL=0FFh
     je   >L1
     inc  cx                    ; change to a true value
     mov  w es:DATA@DV_Version,bx
L1:
     mov  w es:DATA@DV_Loaded,cx
     push cx
     next
end-code

code DV_Pause
     extrn DATA@DV_Loaded:W

     push bp
     mov  bp,sp
     push ds,si,es,di
     mov  ax,w es:DATA@DV_Loaded
     cmp  al,0
     je   >L1                   ; no desqview active
     mov  ax,1000h
     int  15h
L1:
     pop  di,es,si,ds,bp
     next
end-code

code DV_Begin_Critical
     extrn DATA@DV_Loaded:W

     push bp
     mov  bp,sp
     push ds,si,es,di
     mov  ax,w es:DATA@DV_Loaded
     cmp  al,0h
     je   >L1                   ; no desqview active
     mov  ax,101Bh
     int  15h
L1:
     pop  di,es,si,ds,bp
     next
end-code

code DV_End_Critical
     extrn DATA@DV_Loaded:W

     push bp
     mov  bp,sp
     push ds,si,es,di
     mov  ax,w es:DATA@DV_Loaded
     cmp  al,0h
     je   >L1                   ; no desqview active
     mov  ax,101Ch
     int  15h
L1:
     pop  di,es,si,ds,bp
     next
end-code

code DV_Video_Buffer            ; -- VSeg
     extrn DATA@DV_Loaded:W

     push bp
     mov  bp,sp
     push ds,si,di,es
     mov  ah,0Fh                ; determine which video you have
     int  10h
     mov  bx,0B000h             ; if mono (7)
     cmp  al,7h
     je   >L1
     mov  bx,0B800h             ; if color (3)
L1:
     pop  es                    ; ensure we are using the correct ES: value
     push es                    ; after the BIOS call
     cmp  w es:DATA@DV_Loaded,0
;    cmp  al,0h
     je   >L2                   ; no desqview active
     mov  es,bx
     xor  bx,bx
     mov  di,bx
     mov  ax,0FE00h
     int  10h
     mov  bx,es                 ; if alternate DV video segment
L2:
     pop  es,di,si,ds,bp
     push bx                    ; push whichever video segment is correct
     next                       ; exit to next 4th word
end-code

DV_There drop

