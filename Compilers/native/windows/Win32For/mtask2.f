Return-Path: mak@xperts1.rtc.neva.ru 
Received: from [194.85.99.33] by taygeta.com (5.61/1.34)
	id AA18160; Tue, 23 Apr 96 10:45:19 -0700
From: mak@xperts1.rtc.neva.ru
To: skip@taygeta.com
Date: Tue, 23 Apr 1996 21:42 ???
Subject: mtask2 for WIN32FOR
Content-Length: 9354
Content-Type: text/plain
Message-Id: <317d17030.7f8@xperts1.rtc.neva.ru>

file: mtask2.f
\ Utility for multi tasking       version 2            Maksimov M.O.
\                                   email:    mak@informix.rtc.neva.ru
anew  sssss
/*
   Multitasking is reached by maintenance of reaction on event.

    Thus there can arise following problems:
     substitute  data   steck
     substitute  return steck
     substitute  user   variables

The design for substitute user variables has a kind:

   beg-use-vars
    ...                   \  Addresses for substitute
   end-use-vars:  NAME    \  definition of exchangeable word

The design for substitute data steck has a kind:

   n  allot-steck: name   \  n is size of steck
                          \  name  is command of steck substitute

The design for fulfilment of a background program has a kind:

 users{
   ...                    \ Addresses for substitute
  }users                  \ tabl

 offset_for_sp offset_for_rp
          step-task: (name) ( -- f )  \  step of program
           ...            \    background program
             ;

 \ step of program  is   fragment of the program between "PAUSE".

*/

false constant beg-use-vars


: users{ here beg-use-vars ;

: }users begin dup , ?dup
         while   @ ,
         repeat
;

\ : user-xchg   ( tabl -- )
\            begin  dup>r 2@ dup @
\                       r@ cell+ ! !
\                       r> cell+ cell+ dup @ 0=
\            until drop ;

code user-xchg
            mov     eax, [ebx] [edi]
  @@1:      add     ebx,  # cell
            mov     edx, [eax] [edi]
            XCHG   [ebx] [edi], edx
            mov    [eax] [edi], edx
            add     ebx,  # cell
            mov     eax, [ebx] [edi]
            or      eax, eax
            jne     short @@1
            pop     ebx
            next    c;

: end-use-vars: ( 0 adr1 ... adrN -- )
     create }users
\     does>  user-xchg ;
     ;code  lea     ecx, 4 [eax]
            mov     eax, [ecx] [edi]
  @@1:      add     ecx,  # cell
            mov     edx, [eax] [edi]
            XCHG   [ecx] [edi], edx
            mov    [eax] [edi], edx
            add     ecx,  # cell
            mov     eax, [ecx] [edi]
            or      eax, eax
            jne     short @@1
            next    c;

:  allot-steck: create                   \ for
                       sp0 @ dup ,  dup ,
                        -CELLS dup  sp0 ! sp!
                does> dup 2@ { adr sp' sp0' -- }
                      sp@ sp0 @  adr 2!
                      sp' sp! sp0' sp0 ! ;
;
0 value #task
0 value task_flag
0 value task_link
0 constant task_op
0x900 sp0 @ -cells value mt_stacks
-1 cells constant -cell
: #tu-xchg   #task cell+ cell+ @ user-xchg ;

: init-task
                   { staks dest \ r@_ -- }
                     staks 0x400 + cell- to r@_
                   dest rel>abs r@_ !          \ for program
                   -cell +to r@_
                   0x-240  r@_ !               \ for sp
                   -cell +to r@_
                   LP @ r@_ !
                   -cell +to r@_
                   task_op r@_ !
                   0x-40 4 cells - staks !
;

: step-task0:  ( tabl -- )
        ]
        does>   to #task  true to task_flag
                #tu-xchg
                SP@ cell- >R
                LP @ >R
                OP @ >R
                HANDLER @ >R
                RP@ HANDLER !
           #task cell+ @ sp0 @ 0x440 - 0x400 cmove
           #task cell+ @ @
              sp0 @  + RP!
           R>          OP !
           R> sp0 @  + LP !
           R> sp0 @  + SP!
           sp0 @ 0x240 - sp0 !
;
: step-task:  ( tabl -- )
                   here 0x400 allot
        create
                   task_link  >body @ ,
                   last @ name> dup task_link >body !
                                dup to task_link
                   doDefer swap !
                   dup , swap ,
                   here  init-task
                   !CSP ]
;

: (pause)  ( -- 0 )
           sp0 @ 0x240 + sp0 !
           SP@  sp0 @ - >R
           LP @ sp0 @ - >R
           OP @ >R
           #tu-xchg
           sp0 @ 0x440 - #task cell+ @ 0x400 cmove
           rp@  sp0 @ - #task cell+ @ ! 0 to task_flag
                   HANDLER @ RP!
                R> HANDLER !
                R> OP !
                R> LP !
                R> cell+ SP!
                0
;
:Object Tasks-Window <Super Window

 \ -------------------- Get Text Metrics --------------------

 int character-width          \ average width of a character
 int character-height         \ average height of a character

 create tm 14 cells allot

 : get-text-metrics  ( -- )
         tm GetTextMetrics: dc
         tm          @ to character-height
         tm 5 cells+ @ to character-width ;


 \ -------------------- Character Output --------------------

 int CurX        \ Current x position (pixels)
 int CurY        \ Current y position (pixels)

 : AT-XY  ( x y -- )             \ Note this is in character coordinates!!!
         character-height * to CurY
         character-width  * to CurX  ;

 : w-gotoxy  ( x y -- )  to CurY  to CurX  ;     \ Pixel Coords !!

 : w-getxy   ( -- x y )  CurX CurY ;

 : w-type  { addr len -- }
         CurX CurY addr len TextOut: dc
         len character-width * +to CurX ;

 : w-emit  ( char -- )   hld c!  hld 1 type ;

 : w-cr  ( -- )  CurY character-height 4 * + GetSize: self nip  u>
                 if 0  character-height negate scroll: self
                 else  character-height +to CurY
                 then 0 to CurX ;

 : window-io
         ['] w-emit      is emit
         ['] w-type      is type
         ['] w-cr        is cr
         ['] w-gotoxy    is gotoxy
         ['] w-getxy     is getxy
         ;
 : w>forth-io
                ['] _memit      is emit
                ['] _mtype      is type
                ['] crtab       is cr
                ['] _gotoxy     is gotoxy
                ['] _getxy      is getxy
;
 \ Set text colors:

 : foreground  ( color -- )   SetTextColor: dc  ;

 : background  ( color -- )   SetBkColor: dc  ;


:M StartSize:   ( -- w h )      \ the screen origin of our window
                500 200
                ;M

:M On_Init:     ( -- )          \ things to do at the start of window creation
                On_Init: super             \ do anything superclass needs
                0 200 1 hWnd Call SetTimer drop \ init timer to a 200 ms rate
                ;M
:M On_Paint:
                     window-io
                     #task @ execute ?DUP           \
                    IF  DUP 1+ IF MESSAGE THEN
                        doDefer #task  body> !    \ stop
                        0 to task_flag
                        sp0 @ 0x240 + sp0 !
                        #tu-xchg
                        #task   cell+ @
                        #task 3 cells+ init-task
                    THEN
                    w>forth-io
;M
:M Paint:       ( -- )   \ force window repaint
                 0 0 hWnd Call InvalidateRect ?win-error
                ;M

:M WM_TIMER     ( h m w l -- res ) \ handle the WM_TIMER events
                task_flag not
                if   Paint: self                \ refresh the window
                then
                0 ;M

:M On_Done:     ( -- )          \ things to do before program termination
                1 hWnd Call KillTimer drop \ destroy the timer, we are done
                On_Done: super             \ then do things superclass needs
                ;M
:M Start:
                Start: super
                get-text-metrics
                ;M

;Object

addr: Tasks-Window is task_op

 variable xxx
 users{ xxx }users             \ tabl

                   here 0x400 allot
        create     (step_task_noop)
                   last @ name> dup , to task_link
                   dup , swap ,
                   here  init-task
                   HIDE  !CSP
   step-task0:    begin (pause) again ;
 ' (step_task_noop) cell+ to #task


' (step_task_noop) @ constant doTask

\ debug #tu-xchg    abort

: pause task_flag if (pause) else key? drop then ;
: stop task_flag
       if doDefer #task  body> !
       then pause ;
: wake doTask swap ! ;

: local  ( base addr -- addr' )
           >r  cell+ cell+ @
            begin  dup @
                   dup 0= abort" local isn't find"
                      r@ <>
            while  cell+ cell+
            repeat cell+ r>drop ;


: multi start: tasks-Window ;

\s

 variable xx
 users{ xx }users             \ tabl
  step-task: (task)
          0  begin cr dup  .s 1+ ." zzzzzz" pause
                   cr xx  @ .    ." xx"
                   cr dup  .s 1+ ." For continuation: ' (task) wake"
                   cr stop
                   cr dup  .s true abort" For continuation: ' (task) wake"
             again ;
multi

' (task) wake

\s
step_task
55 &OF (step_task) xx local !
step_task
step_task
33 &OF (step_task) xx local !
step_task
step_task
step_task


\s  Test of exchenge addresses

   : .operator  ." operator"  ;
   : .use1      ." use1 " ;
defer .use?

   beg-use-vars
    &OF .use?   base
   end-use-vars: ch-use1

   decimal  ' .operator  is  .use?

   ch-use1  hex   ' .use1  is  .use?
   ch-use1

 cr   0x0a . .use?

   ch-use1 cr 0x0a . .use?
   ch-use1

\s  Test of exchenge steck

0x400 allot-steck: st-use1

 1 2 3 4 5

 st-use1 6 7 8 9
 st-use1
 cr .s
 st-use1 cr .s
 st-use1
\ ?stack      abort cold
