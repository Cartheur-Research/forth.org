
anew  ShowEdit-marker

\ This Modification to EDITING.T file allows you to specify a search 
\ string in addition to loading your editor and editing file through
\ Multi-Forth. It is based on a command tail switch available with the
\ Micro-Emacs editor that I use ( Dan Lawrence's v3.9i ). It can be readily
\ adapted to many other editors with similiar features.

\ I have also re-written SHOW.ERROR so that when an error occurs during
\ file INCLUDEs, the editor is executed and the cursor moves to the
\ offending word pointed to by POCKET. Further enhacements can be made
\ that would high light the word in inverse video. This probably requires
\ writing macros or a command file for this particular editor.
\

\ NOTE: Searching for the error WORD from the beginning of the file may
\ fail to bring up the correct location. For example, the search may come
\ up with a comment.  An alternative is to move to the correct line 
\ number at error time and then search for the string. Unfortunately, my
\ editor will not take both these switches in the command tail. Perhaps
\ a special command file or macro for this editor would solve the problem.
{
: ED  ( -- )	\ Alias for SEDIT. Loads editor and optionally takes
		\ a filename followed by a search string.
		\ EX:  ED filename.t  searchstring
		\ Without any arguments, uses previous filename and no 
		\ search string.

      sedit ;

: SED ( -- )	\ Similiar to ED, but always uses previous file for
		\ editing and next command parsed is a search string.

The editor invoked is Dan Lawrence's MicroEmacs v3.9. A command tail
after the filename with " -S[SearchString] " will search for the string.
See the definition of  (SETSEARCH$) and EDITSEARCH$ to adapt to other
editors.

}
\ *************************************************************************

Create EditSearch$  ,"  -S                                "

: (SetSearch$) (  -- )	 	\ Uses POCKET to find adr of Search String.
				\ Prefix Search String with "-S" and place
				\ in EDITSEARCH$.

	EditSearch$ dup 4+ 32 bl fill  	\ Would be nice to have a string
	32 swap c!			\ package !
	pocket count EditSearch$ 4+ swap cmove 
	EditSearch$ count -trailing 
	EditSearch$ c! drop ;

: SetSearch$   ( -- )   \ parses next word and makes it the Search String.

	 bl word drop (SetSearch$) ;

: NoSearch$  0 EditSearch$ c! ;    \ Make the Search String null.

\ EX:  if packed string is: "THENN"
\      EDITSEARCH$ will contain " -STHENN"

\  1 NewHandle EditingFile	\ Handle already defined from: EDITING.T 

1 NewHandle EditingTail		\ Argument added to command line on
				\ executing the editor.

: EditTail  (  --  )    \ copy filename to EditingTail.
			\ Also append " -S[SearchSTring] " so that editor
			\ will find string after invoking editor.

	EditingTail			\ resize handle to take name
	  EditingFile @ 0$len 		\ Length of EditingFile name
	   2+				\ 1 for NUL, 1 for leading space
	  EditSearch$ c@ +		\ Switches added to argument string
	  resize.handle  ResizeCheck
	EditingTail Clear.Handle	\ erase handle data
	bl  EditingTail @  c!		\ Copy in leading blank
	EditingFile @			\ from passed string.
	 EditingTail @ 1+		\ Skip leading blank
	  over 0$len dup>r		\ Remember length.
	   cmove  			\ Copy into handle.
	EditSearch$ count		\ Append Search String Switch
	 EditingTail @ 1+ r> + 
	   swap cmove ;	
	
: (EDIT)  ( -- )			\ Used once env$,arg$,name$ are
					\ set up for pexec.

	Env$ @				\ env$  for editor
	 EditingTail @ Mask.Handle	\ arg$  for editor
	  EditorName  @ Mask.Handle	\ name$ of editor
	   0				\ mode: execute as sub-program
	pexec				\ execute the program
	?FileError  ;			\ check for success

: SEDIT
	bl word				\ following word is file-name
	dup  1+ c@  bl >  if		\ non-control is first char, use name

		EditingFile #copy.in0	\ Copy filename to handle
		bl word drop  		\ Second item parsed is search$.
		(SetSearch$)

		EditTail		\ set file to edit
	else			
		drop NoSearch$
		EditTail		\ use previous EditingFile
	then				\ ..and no search string.
	(EDIT) ;

: ED  ( -- )	\ Alias for SEDIT. Loads editor and optionally takes
		\ a filename followed by a search string.
		\ EX:  ED filename.t  searchstring
		\ Without any arguments, uses previous filename and no 
		\ search string.

      sedit ;

: SED ( -- )	\ Similiar to ED, but always uses previous file for
		\ editing and next command parsed is a search string.

	bl word				\ following word is file-name
	dup  1+ c@  bl >  if		\ non-control is first char, use name
		(SetSearch$)
		EditTail		\ set file to edit
	else			
		drop 
		EditTail		\ use previous EditingFile
	then				\ ..and previous search string.
	(EDIT) ; 

\ ****************** Now modify SHOW.ERROR ******************************
\
\  ..To invoke the editor and find the offending word...


: file.error   ( line#   --  print file error  )
	?dup if cs cr cr cr cr cr cr
		."   line: "  1- .
		."  WHATIS--> " pocket count type 
                [ 1second 5 * ] literal delay   \ delay before invoking editor
		(EDIT)   \ Invoke editor with command tail for finding
			 \ offending word.

	then  ;

: block.error drop drop ;			\ I never use blocks...


: Cpack  ( adr1 adr2 n -- ) swap 2dup c! 1+ swap cmove ;
   \ Move and pack a string with CMOVE

: show.error    ( general error edit token  )
    blk @	
    ?dup if	dup   block block.error	   \ blocks file load error
    else	?file if		   \ error encountered in a file?
		(SetSearch$)			\ Get offending word from 
						\ Pocket.
		cr ."  file: "			\ print name of file
		input.stream @@ 8+ @		\ get the name handle
		dup   H$Type			\ type the handle text
		@ dup 0$len pad swap cpack	\ IncludeFile at PAD
		pad EditingFile #copy.in0	\ Copy it to EditingFile
		EditTail			\ Set up editing tail.
		input.stream @@ 12 + @		\ line or block number
		tib.size @ 1023 >
		if	1-  tib @ block.error
		else	file.error  then
	then
    then ;
 
token.for show.error edit.token !




