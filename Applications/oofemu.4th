\ The (Dyn)OOF Emulator Version 0.0.2.
\
\ Copyright (C) ZSOTER, Andras 1996.
\
\ This is not the "real thing" just an emulator
\ written in ANS Forth (hopefully), so that people
\ who cannot run my OOF or DynOOF can try out (Dyn)OOF style objects
\ on an ANS-Forth system.
\ I have tested this program on dynoof-0.1.5 (try dynoof -A),
\ on PFE, and on gforth, so probably it is ANS enough that
\ it should run on most ANS-Forth systems.
\ You need the SEARCH ORDER wordset, and probably the minimal
\ requirements (8 wordlists, and 8-deep search order) will not
\ be enough for anything even remotedly useful.
\
\ Unlike in (Dyn)OOF -- { and } cannot be used in interpreter mode!
\ If you want to test an object interactively, use the word O!
\ instead of { to activate the object.
\
\ You can freely distribute, use or abuse this file, as long as
\ I am given credit for it, and you emphasize, that it is just
\ an emulator.
\ You can even modify it, and incorporate it in your own programs
\ as long as the above requirements are met.
\ 
\ For the real thing look at taygeta (taygeta.com or ftp.taygeta.com)
\ /pub/Forth/Reviewed/oof.zip
\ /pub/Forth/Linux/dynoof-0.1.5.tgz
\
\ For some documentation look at my FD papers:
\ http://www.forth.org/fig/andras.html
\ http://www.forth.org/fig/oopf.html


MARKER OOF-EMULATOR

ONLY FORTH DEFINITIONS

\ --------------------------------------------------------------------------
\ The first part is the hierarchical search order emulated by ANS Forth
\ style SEARCH-ORDER words.
\ The basic idea came from Anton Ertl, all the programming errors, bugs
\ and the like are from me.

\ The structure of a vocabulary is:
\ 1st CELL : Wordlist ID
\ 2nd CELL : Pointer to the PARENT vocabulary

: CELL- ( x -- x-CELL ) [ 1 CELLS ] LITERAL - ;

: (Get-Parents) ( Voc -- i*wid i )
   DUP @ SWAP CELL+ @
   DUP IF SWAP >R RECURSE R> SWAP THEN 1+
;



VARIABLE __CONTEXT
VARIABLE __CURRENT 0 __CURRENT !

: (Do-Voc) DUP __CONTEXT !  (Get-Parents) SET-ORDER ;

: (Vocabulary) ( wid +++ )
  CREATE , __CURRENT @ , IMMEDIATE
  DOES> (Do-Voc)
;
  
: VOCABULARY ( +++ ) WORDLIST (Vocabulary) ;

: DEFINITIONS ( -- ) DEFINITIONS __CONTEXT @ __CURRENT ! ;

\ Some more DynOOF style search order words.

: [.] ( -- ) __CURRENT @  (Do-Voc) ;       IMMEDIATE
: [..] __CONTEXT @ CELL+ @ (Do-Voc) ;      IMMEDIATE
: [><] __CURRENT @ DEFINITIONS (Do-Voc) ;  IMMEDIATE

FORTH-WORDLIST (Vocabulary) FORTH
FORTH DEFINITIONS

\ ------------------------------------------------------------------------- 

\ And now comes the OOF emulator.

: Dummy ( -- ) ; IMMEDIATE \ A word which does not do anything.

\ A Class is just a Vocabulary which has a VMT (Virtual Method Table).

VOCABULARY Objects HERE CELL+ ,
3 ,                             \ Maximal # of methods.
0 ,                             \ Current # of methods.
0 ,                             \ The size of an object instance.
0 ,                             \ Parent's VMT.
' Objects >BODY ,               \ This Class.
' Dummy DUP DUP , , ,           \ The methods.

Objects DEFINITIONS

: (UndefinedMethod) ( -- )  1 ABORT" Method Body is Undefined." ;

\ Some constants to make the program more readable.

0                     CONSTANT __WordList
__WordList  CELL+     CONSTANT __Parent
__Parent    CELL+     CONSTANT __VMT

0                     CONSTANT __Maximal#
__Maximal#  CELL+     CONSTANT __Current#
__Current#  CELL+     CONSTANT __Size
__Size      CELL+     CONSTANT __VMTLink
__VMTLink   CELL+     CONSTANT __ThisClass
__ThisClass CELL+     CONSTANT __Methods


: (CopyVMT) ( ThisClass ParentVMT -- NewVMT )
  HERE >R
  DUP @ DUP , >R                        \ Maximal#
  CELL+ DUP @ ,                         \ Current#
  CELL+ DUP @ ,                         \ Size
  CELL+ DUP @ ,                         \ Parent VMT
  CELL+ SWAP  ,                         \ ThisClass
  CELL+
  R> 0 DO I CELLS OVER + @ , LOOP
  DROP R>
;

VARIABLE (OPtr)
VARIABLE (VPtr)


: Class ( #NewMethods +++ )
  VOCABULARY
  HERE DUP CELL+ ,
  2 CELLS - DUP                       \ The address of this class.
  __Parent + @ __VMT + @              \ The Parent's VMT.
  (CopyVMT) __Maximal# + OVER SWAP +! \ The New Maximal # of methods.
  0 ?DO ['] (UndefinedMethod) , LOOP
;

: VmtOf    ( -- Vmt ) __CONTEXT @  __VMT + @ ;
: [VmtOf]  ( -- Vmt ) VmtOf POSTPONE LITERAL ; IMMEDIATE

: (SizeOf) ( VMT -- SizeOf ) __Size + ;
: SizeOf   (     -- SizeOf ) __CONTEXT @ __VMT + @ (SizeOf) ;
: [SizeOf] (     -- SizeOf ) (SizeOf) POSTPONE LITERAL ; IMMEDIATE

: |> ( NewSize --             ) POSTPONE [.] SizeOf ! ;
: <| ( -- Firts-Unused-Offset ) DEFINITIONS SizeOf @ ;
: Nth ( MethodIX -- ) CELLS (VPtr) @ __Methods + + @ EXECUTE ;
: Field ( OldSize -- NewSize ) CREATE DUP , CELL+ DOES> @ (OPtr) @ + ;
: Offset ( Token -- FieldOffset ) >BODY @ ;

: Method ( -- )
  __CURRENT @ __VMT + @
  DUP   __Current# + @
  OVER  __Maximal# + @ < 0= ABORT" Too many methods."
  __Current# + DUP @ DUP CREATE , 1+ SWAP ! DOES> @ Nth
;

Method Done
Method Init
Method Restart

: Index ( Token -- MethodIndex ) >BODY @ ;

: ;M   ( MethodIndex Noname-Sys -- )
  POSTPONE ;
  SWAP CELLS __CURRENT @ __VMT + @ __Methods + + !
; IMMEDIATE

: use: ( MethodIndex -- MethodIndex Noname-Sys ) :NONAME ;
: As ( -- MethodIndex ) ' Index ;

: !O    ( Object -- ) (OPtr) !  ;
: O@    ( -- Object ) (OPtr) @  ;
: !Vmt  ( VMT -- )    (VPtr) !  ;
: Vmt@  ( -- VMT )    (VPtr) @  ;
: O!    ( Object -- ) DUP CELL- @ !Vmt !O ;
: Size@ ( -- Size   ) Vmt@ (SizeOf) @ ;

: MakeObject ( i*x Type Address -- Object )
  Vmt@ O@ 2>R
  OVER !Vmt DUP CELL+ !O ! 
  Init
  O@
  2R> !O !Vmt
;

: (Obj) ( i*x Type ++ )
  CREATE HERE OVER (SizeOf) @ CELL+ ALLOT MakeObject DROP DOES> CELL+
;

: Obj ( i*x ++ ) __CONTEXT @ __VMT + @ (Obj) ;

\ For a "Compile-Only" { } the following definitions will do:

: } ( -- )     POSTPONE 2R> POSTPONE !O POSTPONE !Vmt ; IMMEDIATE
: { ( Obj -- ) POSTPONE Vmt@ POSTPONE O@ POSTPONE 2>R POSTPONE O! ; IMMEDIATE

\ For the digraph notation:

: )> ( -- )     POSTPONE } ; IMMEDIATE
: <( ( Obj -- ) POSTPONE { ; IMMEDIATE

