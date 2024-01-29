#! /usr/local/bin/gforth
\
\  A Forth TAGS generator for analyzing Forth source files
\  output is compatible with the GNU Emacs editor

\ To do:
\    add ability to tag DOES> words and structure/union instances

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
\

\               (c) Copyright 1996, Everett F. Carter Jr.
\                   Permission is granted by the author to use
\		    this software for any application provided this
\		    copyright notice is preserved.

\ $Author: skip $
\ $RCSfile: ftags.fth,v $
\ $Revision: 1.6 $
\ $Date: 1996/04/19 06:22:55 $

\ ===================================================================


\ The following is TRUE for a STANDALONE Forth script
TRUE CONSTANT STANDALONE

\ The following is TRUE for handling FSL structures and unions
FALSE CONSTANT HANDLE_STRUCTURES

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
s" /usr/local/lib/forth/fileio.fth"    included
s" /usr/local/lib/forth/dynmem.fth"    included
s" /usr/local/lib/forth/structs.fth"   included
s" /usr/local/lib/forth/enums.fth"   included

\ ===================================================================


\ states
6 enums: scanning indef quoted incomment instruct inunion

\ state flags
scanning value con_state
false    value recent_eol

  9 CONSTANT TAB       \ the TAB character
 12 CONSTANT FORMFEED_CHAR
 44 CONSTANT COMMA_CHAR
127 CONSTANT DEL

create CRLF   2 allot
create EONAME 1 allot
create COMMA  1 allot
create FORMFEED 1 allot

variable fh          \ the file handle to the current file
variable fout        \ the file handle for the output file

\ statistics on the current file
variable loc         \ lines-of-code (minus comments and blanks)
variable numdefs     \ number of colon definitions
variable numcoms     \ parenthesis delimited comments
variable numlcoms    \ "line" comments, i.e. after a backslash
variable f_string    \ s" type strings
variable c_string    \ c" type strings
variable p_string    \ ." type strings
variable numchars    \ [char] characters
variable current_line
variable file_num

\ support data structures
structure: string
    integer:  .len
    64 chars: .str
;structure

structure: con_table
    integer:              .next              \ linked list pointer
    integer:              .line_no           \ line defined at
    integer:              .file              \ file defined at
     double:              .char_no           \ character number at def
    integer:              .nchars            \ num chars to def end
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
create line_buf    bufsize allot

variable head
variable last-entry
variable flist
2variable current_position

dynamic con_table *entry
dynamic file_list *flist

\ =======================Code===================================

: $. ( 'string -- )
    2dup .len @ >r
         .str r> fout @ write-file
;


: init_stats ( -- )          \ initialize per file statistics
    0 loc !
    0 numdefs !
    0 numcoms !     0 numlcoms !
    0 f_string !    0 c_string !    0 p_string !
    0 numchars !
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
    fh @ file-position drop current_position 2!
    true to recent_eol
;



: print_type ( n dpos -- )

    fh @ reposition-file drop
    line_buf swap 1+ fh @ read-file drop
    line_buf swap 2 - fout @ write-file drop
;


: print_endline ( -- )
    CRLF
    1              \ for MS-DOS use 2 instead of 1
    fout @ write-token
;

: .entry ( con_table -- )

     2dup .nchars @ >R
     2dup .char_no 2@ R> ROT ROT
     print_type

     EONAME 1 fout @ write-token
     2dup .line_no @ fout @ write-uint
     COMMA 1 fout @ write-token
          .char_no 2@ fout @ write-ulong
     print_endline
;

: tag.list ( -- )

    FORMFEED 1 fout @ write-token print_endline
    current_file $. COMMA 1 fout @ write-token loc @ fout @ write-int
    print_endline

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
    0 head !
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


: add-definition ( -- )         \ its a new definition

    1 numdefs +! indef to con_state

    ['] *entry sizeof con_table new

    2dup .next 0 swap !
    2dup .file file_num @ swap !

    dup follow-links !

    2dup .line_no current_line @ swap !

    2dup .char_no >r
    current_position 2@
    false to recent_eol
    scratch_buf fh @ get-token 2drop
    2dup r> 2!
    2swap .nchars >r
     
    fh @ file-position drop
    2swap D-
    drop r> !

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
		       fh @ file-position drop current_position 2!
	               false to recent_eol
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

\ sneaky way to handle definitions
: handle-definition ( 'string -- t )
     2DROP
     [CHAR] : one-char   [CHAR] ; one-char
     true
;

HANDLE_STRUCTURES [IF]

: add-union ( -- )
    indef set? 0= IF
                    add-definition
                    inunion to con_state
    THEN
;

: end-union ( -- )
    inunion set? IF scanning to con_state THEN
;

: add-struct ( -- )
    indef set? 0= IF
                    add-definition
                    instruct to con_state
    THEN
;

: end-struct ( -- )
    instruct set? IF scanning to con_state THEN
;

: check_structure_internals ( c-addr u -- )

     2dup s" array:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" ARRAY:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" cell:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" CELL:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" cells:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" CELLS:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" char:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" CHAR:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" chars:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" CHARS:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" float:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" FLOAT:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" integer:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" INTEGER:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" double:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" DOUBLE:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" pointer:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" POINTER:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" struct:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" STRUCT:" compare
     0= IF handle-definition drop EXIT THEN

 
     2dup s" short:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" SHORT:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" caddr_t:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" CADDR_T:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" long:" compare
     0= IF handle-definition drop EXIT THEN

     2dup s" LONG:" compare
     0= IF handle-definition drop EXIT THEN

    test-last-char

;

: is-struct? ( c-addr -- )      \ handle structure: or STRUCTURE:


    DUP 10 s" STRUCTURE:" compare
    0= IF DROP add-struct EXIT THEN

    DUP 10 s" ;STRUCTURE" compare
    0= IF DROP end-struct EXIT THEN

    DUP 10 s" structure:" compare
    0= IF DROP add-struct EXIT THEN

    DUP 10 s" ;structure" compare
    0= IF DROP end-struct EXIT THEN

    inunion instruct or set? if
        con_state swap
	10 check_structure_internals
        to con_state
    else
    
	10 test-last-char
    then


;


: is-[char]? ( c-addr -- )      \ handle [char] or [CHAR]

    false to recent_eol

    DUP 6 s" [CHAR]" compare
    0= IF scratch_buf fh @ get-token 2DROP DROP
          1 numchars +! EXIT THEN

    DUP 6 s" [char]" compare
    0= IF scratch_buf fh @ get-token 2DROP DROP
          1 numchars +! EXIT THEN

    indef set? 0= IF
        DUP 6 s" create" compare
        0= IF 6 handle-definition  drop EXIT THEN

        DUP 6 s" CREATE" compare
        0= IF 6 handle-definition  drop EXIT THEN
    THEN
  
    DUP 6 s" UNION:" compare
    0= IF DROP add-union EXIT THEN

    DUP 6 s" ;UNION" compare
    0= IF DROP end-union EXIT THEN

    DUP 6 s" union:" compare
    0= IF DROP add-union EXIT THEN

    DUP 6 s" ;union" compare
    0= IF DROP end-union EXIT THEN


    inunion instruct or set? if
        con_state swap
	6 check_structure_internals
        to con_state
    else
    
	6 test-last-char
    then

;

[ELSE]

: is-struct? ( c-addr -- )
    10 test-last-char
;

: check_structure_internals ( c-addr u -- )
    test-last-char
;

: is-[char]? ( c-addr -- )      \ handle [char] or [CHAR]

    false to recent_eol

    DUP 6 s" [CHAR]" compare
    0= IF scratch_buf fh @ get-token 2DROP DROP
          1 numchars +! EXIT THEN

    DUP 6 s" [char]" compare
    0= IF scratch_buf fh @ get-token 2DROP DROP
          1 numchars +! EXIT THEN

    indef set? 0= IF
        DUP 6 s" create" compare
        0= IF 6 handle-definition  drop EXIT THEN

        DUP 6 s" CREATE" compare
        0= IF 6 handle-definition  drop EXIT THEN
    THEN

    6 test-last-char
;

[THEN]


\ handle CONSTANTs, VARIABLEs and VALUES
: constant-or-variable? ( 'string --  t/c-addr len f )
     2dup .len @ >R
          .str r>

     2dup s" CONSTANT" compare
     0= IF handle-definition  EXIT THEN

     2dup s" constant" compare
     0= IF handle-definition  EXIT THEN

     2dup s" VARIABLE" compare
     0= IF  handle-definition  EXIT THEN

     2dup s" variable" compare
     0= IF handle-definition  EXIT THEN


     2dup s" 2CONSTANT" compare
     0= IF handle-definition  EXIT THEN


     2dup s" 2constant" compare
     0= IF handle-definition  EXIT THEN


     2dup s" 2VARIABLE" compare
     0= IF handle-definition  EXIT THEN


     2dup s" 2variable" compare
     0= IF handle-definition  EXIT THEN


     2dup s" VALUE" compare
     0= IF handle-definition  EXIT THEN


     2dup s" value" compare
     0= IF handle-definition  EXIT THEN

     false
;


: otherwise ( 'string -- )

    constant-or-variable? if exit then

    inunion instruct or set? if
        con_state >r
	check_structure_internals
        r> to con_state
    else
	test-last-char
    then

	   
;


: examine ( 'string -- )

    \ not in table, check special cases to see how to handle it
    2dup .len @
    CASE
	1 OF
	    .str c@  one-char
	ENDOF

	2 OF
            .str two-char
	ENDOF

	6 OF
	    .str is-[char]?
	ENDOF

	10 OF
	     .str is-struct?
	ENDOF
    
	drop otherwise 0
    ENDCASE

;


: <process_file> ( -- )

    current_file .str current_file .len @ r/o open-file throw
    fh !

    init_stats

    1 file_num +!
    add-file

    begin           \ loop through all the tokens in the file
	token .str fh @ get-token dup
	0 >
    while
	token .len !
	drop
	token examine
    repeat

    2drop

    tag.list                          \ print tags

    fh @ close-file drop

;

: process_file ( -- )

    ['] <process_file> catch
    if ."   unable to open file: " current_file $. cr then

;


\ ====================The application entry point==================

: tags ( --<file list>--  )

    \ open the output TAGS file
    s" TAGS" w/o create-file throw  fout !

    DEL EONAME c!
    10 CRLF   c!  13  CRLF 1+ c!
    COMMA_CHAR COMMA c!
    FORMFEED_CHAR FORMFEED c!

    ['] eol to eol-handler         \ install end-of-line handler
    0 head !   0 file_num !   0 flist !

    begin                          \ loop through all listed files
        next_file
	dup 0 >
    while
        dup current_file .len !        \ save the file name
	current_file .str swap cmove
        0. current_position 2!
	process_file
    repeat

    2drop

    0 to eol-handler                 \ de-install EOL handler
    fout @ close-file drop

;


STANDALONE [IF]
 tags bye
[THEN]





