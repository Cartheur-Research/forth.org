\ Utility for multi tasking for Win32For                 Maksimov M.O.
\                                   email:    mak@informix.rtc.neva.ru

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
          stap-task: (name) ( -- f )  \  stap of program
           ...            \    background program
             ;

 \ spap of program  is   fragment of the program between "PAUSE".

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

: #tu-xchg   #task cell+ @ user-xchg ;

: stap-task:  ( tabl offset_for_sp offset_for_rp -- )
        create  sp0 @ -cells
                   rp@ to #task  dup rp!
                   here cell+ cell+ rel>abs >r
                  -cells >r            \  sp
                    LP @ >R
                    OP @ >R
                    rp@ , ,
                   #task rp! 0 to #task
                     HIDE  !CSP ]
        does>   dup is #task
                #tu-xchg
                SP@  >R
                LP @ >R
                OP @ >R
                HANDLER @ >R
                RP@ HANDLER !
           @  RP!
           R> OP !
           R> LP !
           R> SP!
;
\ debug stap-task:    \ abort
: ?pause
      SP@ cell+ >R
           LP @ >R
           OP @ >R
           #tu-xchg
           rp@ #task !  0 to #task
                   HANDLER @ RP!
                R> HANDLER !
                R> OP !
                R> LP !
                R> SWAP >R
                SP! DROP
                R>
;

: pause #task if 0 ?pause then ;

\s

 users{ sp0 }users             \ tabl

 1000 1000 stap-task: (stap_task)  sp@ sp0 !
          0  begin cr dup  .s 1+ ." zzzzzz" pause
                   cr dup  .s 1+ ." ssssss" pause
             again ;

0 value ?stop

: stap_task ?stop  ?exit
            (stap_task) ?DUP
        IF      DUP 1+ IF MESSAGE THEN
                #tu-xchg  true to ?stop
        THEN  0 to #task
;

stap_task
stap_task
stap_task
stap_task
stap_task
stap_task


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

 1000 allot-steck: st-use1

 1 2 3 4 5

 st-use1 6 7 8 9
 st-use1
 cr .s
 st-use1 cr .s
 st-use1

