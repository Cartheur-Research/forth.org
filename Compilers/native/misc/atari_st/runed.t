
{
    This program's main function is essentially that of SHOWED.T, mainly to
directly run a subprogram - such as an editor - normally executable from the
DESKTOP, without exiting to the DESKTOP from MultiForth. My enhancements are
as follows:

o  To run GEM programs that utilize windows ,icons, mouse pointers ( WIMP,
   for short ) such as 1ST_WORD.PRG, as well as non-GEM text based editors.
   Also, to my amazement, 1ST_WORD can be invoked with a command tail
   just like MicroEMACS 3.9, but without the search string feature,
   unfortunately.

o  Some changes are fluff, but the essential features will automatically:

   - selects the drive BEFORE changing the subdirectory path to where your
     subprogram resides. This is nessesary for ANY program that search for 
     resources ( 1STWORD ) or overlays ( MicroEMACS 3.9-1/4 ).

   - correctly restores the resource pointer, which 1stWord doesnt. Otherwise,
     as you run another program that doesn't point to it's own, but issues
     a screen update using the prior pointer left dangling by an earlier GEM
     program, you're sure to crash.

   - correctly closes the current file before PEXECing the editor. The reason
     is because of the old SHOW.ERROR jumped into the editor without first
     closing the file's handle, and everything seemed normal. But if you save
     the file you edited, the old file doesn't get overwritten, and you'll
     discover two files of THE SAME NAME in the same directory. File handles
     must always be closed when your through with them or when PEXECing a 
     child process.
   
   - displays the TIB upon encountering an error while compiling a file.
     Unlike SHOWED.T, RUNED.T doesn't jump into the editor immediately. But it
     does set the command tail, and waits for a keypress. The reason is that
     MicroEMACS version 3.9-1/4 downloaded from Genie and Compuserve only
     acknowledges a command tail of 32 characters, cutting off the rest.
     That means you're limited in your file's subdirectory depth and search
     string length, which combined,  must both be contained within 32
     characters. This will cut off your search string making for a more un-
     reliable search. Sorry, I dont have a Hotline to Dan Lawrence! I made
     changes to SHOW.ERROR to display the contents of the TIB so that you
     can intelligently search for a whole phrase. This is to benefit those 
     who's editors dont have line number GOTO capabilities, where a single
     word search fails upon a match in a comment region or MicroEMACS 32
     character bug.

o  One doesn't need to specify a full pathname when invoking the editor to
   edit a file in the current directory. ED will build it for you. The search
   pattern is specified as such:

     root:               ed \readme, or, ed d:\readme
     default directory:  ed readme, or, ed d:readme
     subdirectory:       ed f:\multi4th\library\runed.t
             
   This is good practice in any application because it forces TOS to look
   where you specify. This also follows the pattern specification for the
   Multiforth word 'LS' used for listing directory information.

    That pretty much sums up my enhancement's. You must edit the INCLUDE 
directive to reflect where you put the file STR0.T, and make sure you have 
alloted about 1000 bytes of object space prior to compiling 
( 1000 minimum.object ).
    I can be reached for comments or questions on anything FORTH or GEM
related on Compuserve or GEnie via E-MAIL. Enjoy.

                                                Robert Gillies
                                                Compuserve: [72037,1626]

******************************************************************************
}


anew runed-marker

include f:\multi4th\library\str0.t     \ edit this to point to where STR0.t is

variable prevdrv

1 newhandle editingtail

1 newhandle searchhandle

\ 1 newhandle editingfile  defined in EDITING.T


: cpack   ( addr1 addr2  n -- )   swap 2dup c! 1+ swap cmove ;


: uptake   ( -- )    pad 1+ str0cat pad 1+ 0$len pad c! ;


: AppendSearch           ( -- )

                pad off
                editingtail @ uptake
                0"  -S" uptake
                searchhandle @ uptake
                pad editingtail #copy.in0 ;



: RemoveWildCard  ( 0$addr -- )    \ remove back to a '\'
     
     dup  dup 0$len  +             \ start searching at string end
      do                           \ descending loop
         Ic@  ascii \  =           \ search back for "\"
         if   leave                \ string is complete
         else   0 Ic!              \ put 0 over non-\ characters
         then
          -1 +loop  ;

 


: (edit)                ( -- )

        pad editingtail #copy.in0          \ the command tail minus the search$

        getdrive prevdrv !                      \ save my drive #
        editorname @ mask.handle
        dup 0$len upper                     \ transpose editorname to uppercase
        editorname @
        dup 1+ c@ ascii : =                     \ do I select my editor's path?

        IF      
                c@ 64 - setdrive                \ yes I do, therefor....
                editorname @ pad str0copy
                pad removewildcard pad (cd)
        ELSE
                drop                            \ no, I dont have to.
        THEN

    
        editorname @ mask.handle
        dup 0$len + 4 - 4
        " .PRG" count match drop not             \ am I a GEM editor?

        IF
              cursoroff initcursor               \ yes I am
        ELSE
              AppendSearch                 \ no, so I'll add the search string
        THEN
  

\ ***** at this point the command tail for PEXEC is finalized. *******

        cs
        editorname @ mask.handle          \ Now for some fluff.
        dup 0$len dup 2/ negate
        GetRez
        IF 80 ELSE 40 THEN
        2/ home + spaces type             \ pretty print the editor's name. 
        cursoroff
        
                env$ @
        editingtail @ mask.handle
        editorname  @ mask.handle
        0 pexec                            \ LET'S GO
        prevdrv @ setdrive
        0 0 14 WSet$                 \ no dangling resource pointers, PLEASE!
        hidecursor cs cursoron
        ?fileError ;


: AddTheSearch   ( -- )

        pocket searchhandle #copy.in0 ;        \ copy POCKET to handle. 


: AddTheCommand  ( -- )

        pocket 0terminate uptake               \ uptake the new file.
        pad editingfile #copy.in0              \ replace old file with it.
        bl word drop AddTheSearch ;            \ stuff the search $ at POCKET.


: ED          ( -- )  \ parse both a file string, and it's search string
                      \ if neither present, use previous file, with no search.

                pad off
                0"  " uptake             \ TOS needs The Leading Blank (TLB)
                bl word drop true               \ stuff POCKET with command$
        CASE
                pocket 2+ c@ ascii : =          \ Incaseof a drive specifier?
        OF
                AddTheCommand                   \ good enough.
                ENDOF
                
                pocket 1+ c@ ascii \ =          \ Incaseof my own root?
        OF
                getdrive 64 + pad 2+ c!         \ my drive #
                0" :" uptake                    \ the drive pattern
                AddTheCommand                   \ good enough.
                ENDOF
                
				pocket 1+ c@ bl >               \ Incaseof a name only?
        OF
                pad 2+ (pwd) 0" \" uptake       \ gotta build the path first.
                AddTheCommand                   \ good enough.
                ENDOF
                
                editingfile @ uptake            \ Ifnoneabove, use old file.
        ENDCASE
        (edit) ;
                
: sed           ( -- )  \ parse a search string only, using previous filename.
                        \ if none parsed, use old file and old search$.

       pad off editingfile @ uptake             \ previous filename
       bl word 1+ c@                            \ search string?
       IF  AddTheSearch THEN (edit) ;           \ ifso, copy POCKET to handle



\ ****************** Now modify SHOW.ERROR ******************************
\
\  ..To invoke the editor and find the offending word...


: file.error   ( line#   --  print file error  )
        ?dup if 
                cr ." Undefined word at line # " .
                cr cr tib @ tib.size @ type                             
                cr pocket  c@ 1+ >IN @ over - spaces 
                1- 0 MAX 0 DO ascii ^ emit LOOP
                cr ." Command tail with search string set."
                cr ." Type any key to start editing."
                CON: devkey drop (edit)
        then  ;

: block.error drop drop ;                       \ I never use blocks...


: show.error    ( general error edit token  )
    blk @       
    ?dup if     dup   block block.error    \ blocks file load error
    else        ?file if                   \ error encountered in a file?
                cr ." Error in file: "     \ print name of file
                
                pad off             \ build the *full* pathname
                0"  " uptake        \ TLB
                pad 2+ (pwd)        \ place working directory past TLB.
                0" \" uptake
                infile @ close      \ ALWAYS close files when done
                                    \ or before PEXECing    
                
                input.stream @@ 8+ @       \ get the name handle
                dup   H$Type               \ type the handle text
                @ uptake                   \ IncludeFile at PAD
                AddTheSearch   

                pad Editingfile #copy.in0       \ Copy it to EditingFile
                input.stream @@ 12 + @          \ line or block number
                tib.size @ 1023 >
                if      1-  tib @ block.error
                else    file.error  then
        then
    then ;
 
token.for show.error edit.token !



{ ***********************************************************************

    A version of WHEREIS.T that is independant of SHOWED.T. This version does
 not set the command tail. It is useless to set the command tail when the
 file's drive and path is no longer known, especially to hard drive owners
 like myself who rarely are ever in the same subdirectory of the file's 
 origin. Be aware that it only searches the context vocabulary, so you must
 switch to different vocabularies to do an extensive search.

************************************************************************}



: Marker-List  ( -- )   \ list words and tokens in CONTEXT vocabulary
                        \ with the "-marker" extension.
        Context @ ?dup if
          @ ?dup if             \ is valid, non-purged handle
                dup  @ +        \ point to vocab data
                begin
                        KeyStop?        \ respond to keyboard actions
                        dup >w@<        \ at end of vocabulary?
                    while
                        2+ count  31 and        \ removes precedence bits
                        2dup
                        +  7 -                  \ points to "-" in marker
                        " -MARKER" count rot
                        -text 0= if
                        2dup cr type  over 3- >w@< 7 .r  then
                        +                       \ next name in vocab
                repeat
                drop
        then  then  ;


{  Latest count drop 3- >w@<    ...returns token }

: Marker-Find ( nfa1 -- adr cnt | of "*-marker" name field )

        \ Search through name fields in order to find the place marker
        \ compiled at beginning of source file containing the filename.
        \ NFA1 is the name field of the word we are trying to find
        \ source file for.  ADR CNT is the name field of the place 
        \ marker.
                2-                      \ Move 2 bytes back to token field
                begin
                        dup >w@<        \ at end of vocabulary?
                while
                        2+ count  31 and        \ removes precedence bits
                        2dup 
                        +                       \ pointing to end of name
                        7 -                     \ points to "-" in "-marker"
                        " -MARKER" count rot
                        -text 0= if             \ String match?
                                        exit
                                 then
                        +                       \ next name in vocab
                repeat
                drop ;


: whereis ( -- )        \ Look up following word and find the nearest
                        \ "-marker" word in dictionary. Convert to source
                        \ filename, and display it's name.  

  
        [compile] token.for
        context @ >name
        marker-find
        pad swap cpack
        pad count dup>r + 7 -
        dup ascii . swap c!
        1+    ascii T swap c!
        r> 5- pad c!
        3 spaces pad count
        11 min type ;


