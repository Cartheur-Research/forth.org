#! /usr/local/bin/gforth
\
\  A Forth concordance generator for analyzing Forth source files

\  This is a ANS Forth program requiring:
\       1. The CORE EXTENSIONS wordset words: CASE OF ENDOF ENDCASE
\                                     TRUE FALSE TO VALUE and \
\       2. The STRING wordset words:  CMOVE and COMPARE
\       3. The FILE wordset
\       4. The MEMORY ALLOCATION wordset.
\       5. The EXCEPTION wordset.
\       6. Heterogeneous data structure words from the Forth
\          Scientific Library
\       7. ASCII FILE I/O words from the Forth Scientific Library
\       8. The standalone version requires access to the command
\          line arguments, the GFORTH version is implemented here

\               (c) Copyright 1996, Everett F. Carter Jr.
\                   Permission is granted by the author to use
\		    this software for any application provided this
\		    copyright notice is preserved.

\ $Author: skip $
\ $RCSfile: concordance.fth,v $
\ $Revision: 1.6 $
\ $Date: 1996/04/18 00:14:45 $

\ The following is TRUE for a STANDALONE Forth script
TRUE CONSTANT STANDALONE

STANDALONE [IF]

variable f_index   2 f_index !     \ 2 to jump past gforth invocation and condordance.fth itself


: next_file ( -- c-addr u )

    \ ." argc " argc @ .  ." f_index " f_index @ . CR
    
    argc @ f_index @ - 1 < if
	0 0 
    else
	f_index @ arg
        1 f_index +!
    then

;


[ELSE]

: next_file ( -- c-addr u )
    bl word count
;

[THEN]

\ ==============Support code from the Forth Library==================

s" /usr/local/lib/forth/fsl-util.nofloat.fth" included
s" /usr/local/lib/forth/dynmem.fth"    included
s" /usr/local/lib/forth/structs.fth"   included
s" /usr/local/lib/forth/fileio.fth"    included
s" /usr/local/lib/forth/enums.fth"   included

\ ===================================================================

\ states
4 enums: scanning indef quoted incomment

\ state flags
true     value verbose
scanning value con_state


variable fh          \ the file handle to the current file

\ statistics on the current file
variable loc         \ lines-of-code (minus comments and blanks)
variable numdefs     \ number of colon definitions
variable numcoms     \ parenthesis delimited comments
variable numlcoms    \ "line" comments, i.e. after a backslash
variable f_string    \ s" type strings
variable c_string    \ c" type strings
variable p_string    \ ." type strings
variable numchars    \ [char] characters
variable numvars     \ number of variables (and 2variables)
variable numcons     \ number of constants
variable numvals     \ number of values    
variable current_line
variable file_num

\ support data structures
structure: string
    integer:  .len
    64 chars: .str
;structure

structure: ref_table                        \ table of references
    integer: .ref_next
    integer: .ref_file     \ the file name index
    integer: .ref_line
;structure

structure: con_table
    integer:              .next              \ linked list pointer
    sizeof string struct: .name              \ the name of the def
    integer:              .line_no           \ line defined at
    integer:              .file              \ file defined at
sizeof ref_table struct:  .references        \ lines referenced at
;structure


structure: file_list
    integer:              .next_file
    sizeof string struct: .fname
;structure

\ misc variables

string current_file
string token

128 constant bufsize
create scratch_buf bufsize allot

variable head
variable last-entry
variable flist

dynamic con_table *entry
dynamic ref_table *ref
dynamic file_list *flist

\ =======================Code===================================

: $. ( 'string -- )
    2dup .len @ >r
         .str r> type
;

: init_stats ( -- )          \ initialize per file statistics
    0 loc !
    0 numdefs !
    0 numcoms !     0 numlcoms !
    0 f_string !    0 c_string !    0 p_string !
    0 numchars !    0 numvars  !    0 numcons  ! 0 numvals !
    1 current_line !

;

: set_state ( x -- )
    con_state or to con_state
;

: clear_state ( x -- )
    invert con_state and to con_state
;

: set? ( x -- t/f )
    con_state and
;

: eol ( -- )                  \ end-of-line handler for fileio words
    indef set? if 1 loc +! then
    1 current_line +!
;

: .file_num ( n -- )            \ print a particular file name
                                \ presumes that n will be legal

    dup file_num !
    flist @ swap
    1 ?do
         (struct file_list *) swap .next_file @
       loop

       (struct file_list *) swap .fname $. ."   "
;

: ref-print ( 'ref_table -- )

    2dup .ref_line @ 0= if 2drop exit then   \ no other refs
    ." ,references: "

    0          \ stack a flag to control deleting reference
               \ structures (do not want to delete the first one)
    begin
      >r
      2dup .ref_file @
      dup file_num @ <> if cr ."     " .file_num else drop then

      2dup .ref_line @ .
      2dup .ref_next @
      r> if
          >r
          ['] *ref struct!
          ['] *ref delete-struct
	  r>
          else
            >r 2drop r>
	  then
      dup
    while
	(struct ref_table *) swap
        -1
    repeat

    drop
;

: .entry ( 'con_table -- )

     2dup .name $.
     ."     ,definition at: "
     2dup .file @ .file_num
     2dup .line_no @ .

    .references ref-print
    cr
;

: .table ( -- )


    head @
    begin
      dup
    while
      (struct con_table *) swap
      2dup .file @
      file_num !    \ re-using file number variable to control
                    \ when to print file name

      2dup .entry
      2dup .next @ >r
      ['] *entry struct!
      ['] *entry delete-struct
      r>
    repeat

    drop

;

: file-stats ( -- )
    cr
    ." File: " current_file $. cr
    ." Approximate Lines of code: " loc @ . cr
    ." number of definitions: " numdefs @ . cr
    ." number of variables: " numvars @ . cr
    ." number of values: " numvals @ . cr
    ." number of constants: " numcons @ . cr
    ." number of comments: " numcoms @ .
    ."   line comments: " numlcoms @ . cr
    ." number of quoted characters: " numchars @ . cr
    f_string @ . ." Forth strings    "
    c_string @ . ." counted strings     "
    p_string @ . ." print strings " cr cr

;

: .flist ( -- )                 \ print the entire file list
    flist @
    begin
      dup
    while
	(struct file_list *) swap
	2dup .fname $. cr
             .next_file @
    repeat
    drop
;


: follow-links ( -- addr )      \ follow links and get address
                                \ of last list address

    head head @
    begin
      dup
    while
      swap drop
      (struct con_table *) over .next @
    repeat

    drop
  
;

: add-file ( -- )

    flist flist @
    begin
	dup
    while
	swap drop
	(struct file_list *) over .next_file @
    repeat

    drop

    ['] *flist sizeof file_list new

    current_file 2dup .len @ >r
                      .str r>

    2over .fname .str swap dup >r cmove

    2dup  .next_file 0 swap !
    2dup  .fname .len r> swap !

    swap drop swap !
;

: add-reference ( 'ref_table -- )

    

    2dup .ref_line @ 0=
    if 2dup .ref_file file_num @ swap !
	    .ref_line current_line @ swap ! exit then

    begin
      2dup .ref_next @
      dup
    while
      >r 2drop 
      (struct ref_table *) r>
    repeat

    drop
    .ref_next

    ['] *ref sizeof ref_table new
    
    2dup .ref_line current_line @ swap !
    2dup .ref_next 0 swap !
    2dup .ref_file file_num @ swap !

    swap drop swap !
;

: add-definition ( -- )         \ its a new definition
    1 numdefs +! indef to con_state

    ['] *entry sizeof con_table new

    2dup .next 0 swap !
    2dup .file file_num @ swap !
    2dup .references 2dup .ref_next 0 swap !
                          .ref_line 0 swap !

    dup follow-links !

    2dup .line_no current_line @ swap !
    2dup .name .str fh @ get-token
    swap drop >r
         .name .len r> swap !
;

: one-char ( c -- )      \ handle special 1-character tokens
                         \ " ) : ; \ and (

    quoted set? IF [CHAR] " = IF quoted clear_state THEN EXIT THEN

 incomment set? IF [CHAR] ) = IF incomment clear_state THEN EXIT THEN

    CASE
  	  [CHAR] : OF             \ a new definition starts
	             indef set? 0= IF add-definition THEN
		   ENDOF

          [CHAR] ; OF             \ done with a definition
	             indef set? IF scanning to con_state THEN
	           ENDOF
      
	  [CHAR] \ OF	           \ skip any line comments
	            scratch_buf bufsize fh @ read-line
		    drop 2drop
		    1 numlcoms +!
                    1 current_line +!
		   ENDOF

           [CHAR] ( OF              \ start of a comment
	             incomment set_state
                     1 numcoms +!
	  	 ENDOF

  ENDCASE

;

\ check to see if last char is a quote or end of ) comment

: test-last-char ( c-addr u -- )

    + 1- c@

	 CASE
	     [CHAR] " OF
	               quoted set? IF quoted clear_state THEN
                      ENDOF
 
             [CHAR] ) OF
		       incomment set? IF con_state clear_state THEN
		    ENDOF

         ENDCASE
;

: two-char ( c-addr -- )   \ handle special 2-character tokens
                           \ S" s" C" c" ." and .(

    [CHAR] " scratch_buf 1+ c!

    DUP 2 [CHAR] S scratch_buf c! scratch_buf 2 compare
    0= IF quoted set_state DROP 1 f_string +! EXIT THEN
    DUP 2 [CHAR] s scratch_buf c! scratch_buf 2 compare
    0= IF quoted set_state DROP 1 f_string +! EXIT THEN

    DUP 2 [CHAR] C scratch_buf c! scratch_buf 2 compare
    0= IF quoted set_state DROP 1 c_string +! EXIT THEN
    DUP 2 [CHAR] c scratch_buf c! scratch_buf 2 compare
    0= IF quoted set_state DROP 1 c_string +! EXIT THEN

    DUP 2 [CHAR] . scratch_buf c! scratch_buf 2 compare
    0= IF quoted set_state DROP 1 p_string +! EXIT THEN

    DUP 2 [CHAR] ( scratch_buf 1+ c! scratch_buf 2 compare
    0= IF incomment set_state DROP 1 p_string +! EXIT THEN


    2 test-last-char
	   
;

: is-[char]? ( c-addr -- )      \ handle [char] or [CHAR]

    DUP 6 s" [CHAR]" compare
    0= IF scratch_buf fh @ get-token 2DROP DROP
          1 numchars +! EXIT THEN

    DUP 6 s" [char]" compare
    0= IF scratch_buf fh @ get-token 2DROP DROP
          1 numchars +! EXIT THEN

    6 test-last-char
;

\ sneaky way to handle definitions
: handle-definition ( 'string -- t )
     -1 numdefs +!   \ dont want to count constants, values, variables
     2DROP
     [CHAR] : one-char   [CHAR] ; one-char
     true
;

\ handle CONSTANTs, VARIABLEs and VALUES
: constant-or-variable? ( 'string --  t/c-addr len f)
     2dup .len @ >R
          .str r>

     2dup s" CONSTANT" compare
     0= IF 1 numcons +! handle-definition EXIT THEN

     2dup s" constant" compare
     0= IF 1 numcons +! handle-definition EXIT THEN

     2dup s" VARIABLE" compare
     0= IF  1 numvars +! handle-definition EXIT THEN

     2dup s" variable" compare
     0= IF 1 numvars +! handle-definition EXIT THEN


     2dup s" 2CONSTANT" compare
     0= IF 1 numcons +! handle-definition EXIT THEN


     2dup s" 2constant" compare
     0= IF 1 numcons +! handle-definition EXIT THEN


     2dup s" 2VARIABLE" compare
     0= IF 1 numvars +! handle-definition EXIT THEN


     2dup s" 2variable" compare
     0= IF 1 numvars +! handle-definition EXIT THEN


     2dup s" VALUE" compare
     0= IF 1 numvals +! handle-definition EXIT THEN


     2dup s" value" compare
     0= IF 1 numvals +! handle-definition EXIT THEN

     false
;

: otherwise ( 'string -- )
    constant-or-variable? if exit then

    test-last-char
;

: ?in-table ( 'string -- t/f )    \ is it in the table ?

    0 last-entry !

    head @ 0= if 2drop false exit then

    2dup .len @ >r
         .str r>

    head @
    begin
      dup
      if
	  (struct con_table *) over .name
	  2dup .len @ >r
	       .str r>
	       4 pick 4 pick compare
          0= if last-entry ! 0 else -1 then
      then
    while
      (struct con_table *) swap
      .next @
    repeat

    2drop

    last-entry @ if true else false then


;

: examine ( 'string -- )

    \ test to see if its already in the table, if so then add
    \ to the reference list
    
    2dup ?in-table if 2drop (struct con_table *) last-entry @
	              .references add-reference exit then


    \ not in table, check special cases to see how to handle it
    2dup .len @
    CASE
	1 OF
	    2dup .str c@  one-char
	ENDOF

	2 OF
           2dup .str two-char
	ENDOF

	6 OF
	    2dup .str is-[char]?
	ENDOF
    
	drop 2dup otherwise 0
    ENDCASE


    2drop

;


: <process_file> ( -- )
    verbose if
	." processing file: " current_file $.
    then

    current_file .str current_file .len @ r/o open-file throw
    fh !

    init_stats

    1 file_num +!
    add-file

    verbose if
	cr
    then

    begin           \ loop through all the tokens in the file
	token .str fh @ get-token dup
	0 >
    while
	token .len !
	drop
	token examine
    repeat

    2drop

    fh @ close-file drop

    file-stats

;

: process_file ( -- )

    ['] <process_file> catch
    if ."   unable to open file: " current_file $. cr then

;

\ ====================The application entry point==================

: concordance ( --<file list>--  )

    cr

    ['] eol to eol-handler         \ install end-of-line handler
    0 head !   0 file_num !   0 flist !

    begin                          \ loop through all listed files
        next_file
	dup 0 >
    while
        dup current_file .len !        \ save the file name
	current_file .str swap cmove
	process_file
    repeat

    2drop

    \ .flist

    .table                          \ print concordance

    0 to eol-handler                 \ de-install EOL handler

;



STANDALONE [IF]
 concordance bye
[THEN]



