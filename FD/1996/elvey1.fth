cmForth and Special NC4000 Words Used@+ ( Addr Incr - Value Addr+Incr )Machine coded fetch and increment.!+ ( Value Addr Incr - Addr+Incr )Machine coded store and increment.FOR ( n - )FOR NEXT loop is like DO LOOP, except the loop counter NEXT is a down counter. The loop will execute n+1 times. When the value on the return stack = 0, NEXT will cause the loop to stop. You�ll see places in my code where I do R> DROP 0 >R. This is the same as LEAVE in some Forths.BSWAP ( HL - LH )Not actually a cmForth word. I added special hardware to speed up byte swapping in a 16-bit value.I  ( - Value )I in cmForth is like R@. It simply copies the top of the return stack. The return stack is also used for the FOR NEXT counter, so it makes more sense to call it I.DR0 ( - )Selects the offset for drive 0.	DR0 = 360K Floppy A	DR1 = 360K Floppy B	DR2 = 5 Mb HDTIMES ( n - | Word )Executes the following word n+1 or n+2 times. This is a confusing one because it works differently if the word is a code word than if the word is a nested word. Code words are execute n+2 times.2/MOD ( n - r q )DOES ( - )Similar to DOES> in regular Forth, but doesn�t return the address of the parameter field. To get the parameter field, it must be popped from R with R>. Since the carry bit may have been pushed into the high bit, it also needs 7FFF AND. Esentially:   DOES>  equals  DOES R> 7FFF AND DECIMAL\ The NC4000 is a 16-bit, word addressed machine. Because of\  this and the fact that I wanted to use two bytes to an\  address space, I made a few words to assist.: 2C@+ ( Addr - Addr+1 Low High )   \ two bytes from word location  1 @+                   \ Fetch 16 bits and incr pointer  SWAP DUP 255 AND       \ Low part  SWAP BSWAP 255 AND ;   \ Hi partHEX\ SMASH and UNSMASH are like MOVE and CMOVE combined.: SMASH ( From To - ) ( 512 Bytes )    \ Packs 2 bytes into a word  2* 1FF FOR     \ Byte addresses are twice     >R     1 @+         \ Fetch from and incr     SWAP 0FF AND \ Low byte     I C!         \ Store in to     R> 1 +       \ Incr to  NEXT DROP DROP ;: UNSMASH ( From To - ) ( 512 Bytes )   \ UnPacks a word to two bytes  0FF FOR     >R                \ Easier with one out of the way     2C@+ 4000 OR I !  \ Same format as for disk     4000 OR           \  used in CMForth: 40xxH     R> 1 + 1 !+       \ Incr and store  NEXT DROP DROP ;: DOS0  FLUSH DR0 ; \ always use DR0 for DOS diskCREATE FAT 200 ALLOT  ( 1024 BYTES ) \ Storage for a FAT from disk\ FAT ( file allocation table ) contains linked list that describes\  the usage of the disk. A DOS disk has two copies in the hopes\  that only one might be damaged, allowing recovery of the data on\  the disk. On a 360K disk, the first FAT is at sector 3 ( zero based )\  The second is at sector 5. Reading BLOCK one gets part of each,\  but they are out of order, hence the fun offsets below.: RDFAT ( - ) ( read FAT )   \ Easier to read last part of first FAT and first part   \  of second with block = 1K   DOS0 1 BLOCK                  \ used block I/O rather than direct read   DUP 200 + FAT SMASH           \ do first 512 bytes from 2nd FAT   [ FAT 100 + ] LITERAL SMASH ; \ do next 512 bytes from 1st FATCREATE DRCT    100 7 * ALLOT  \ Space for the root directory\ I only provide usage of the root directory. Sub-directories\  could be easily added.: RDDIR ( read DIR )  \ Transfer a copy of the root directory to memory     DOS0     2 BLOCK 200 + DRCT SMASH     3 BLOCK DUP DRCT 100 + SMASH           200 + DRCT 200 + SMASH     4 BLOCK DUP DRCT 300 + SMASH           200 + DRCT 400 + SMASH     5 BLOCK DUP DRCT 500 + SMASH           200 + DRCT 500 + SMASH ;  HEX\ The FAT points to a cluster of sectors. On the 360K disk a\  cluster is 2 sectors. To make things tougher, the entries\  in the 360K FAT are two 12-bit values packed into 3 bytes.\  This requires a fancy fetch. The value fetched can be one of\  several values. 0 = unused, 0FF8 to 0FFF is an end marker,\  0FF7 is a bad cluster, 0FF0 to 0FF6 are reserved, and 0002\  thru 355 decimal are pointers to the next cluster. : FAT@ ( Cluster - Cluster' )  \ Given a cluster number, fetch the next link with a 12-bit FAT   2/MOD 3 *      \ Calc bytes offset   FAT 2* +       \ Make byte address in FAT   >R IF          \ If odd cluster     1 I + C@       \ Fetch bits 0-3     2 TIMES 2/     \ Shift right by 4  ( 2/ is code word )     R> 2 + C@      \ Fetch bits 4-11     2 TIMES 2* OR  \ Shift left by 4 and combine ( 2* is code word )   ELSE           \ If even cluster     I C@           \ Bits 0-7     R> 1 + C@      \ Bits 8-11 but in position 4-7     BSWAP OR       \ So swap and combine   THEN   0FFF AND ;      \ Mask off unwanted part: C@+ ( A - V A' )     DUP C@ SWAP 1 + ;: CLSTR/MOD ( Low High - Rem Clstr# )\  32-bit file size to cluster and part   BSWAP 4 TIMES 2* >R  \ Shift left by 6   BSWAP DUP 3FF AND    \ Remaining amount   SWAP 8 TIMES 2/      \ Shift right by 10   3F AND R> + ;        \ Make cluster number   0E0 1 - CONSTANT #DIRS  \ number of directory entries less one: DIR    \ display directory \ Name Ext Clusters RemBytes FisrtCluster Attribute \ \ One could add printouts. I ignored the attributes and date \  since I had no real-time clock when I wrote \  this and I wasn't checking attributes. This assumes that \  the directory has already been read into RAM with RDDIR. If \  one wanted to, they could add RDDIR but one would have to \  be careful to write out any updates before using it or have \  a directory-updated flag    DRCT 2*         \ Byte address    #DIRS FOR       \ 112 directory entries for 360K disk     DUP C@ DUP 0E5 = NOT AND  \ 0 = empty, 0E5 = erased     IF       CR 7 FOR         C@+ SWAP EMIT  \ Filename 8 characters       NEXT SPACE       2 FOR         C@+ SWAP EMIT  \ Extension 3 characters       NEXT       C@+        \ Attribute       0E + 2/    \ Back to 16-bit addressing       1 @+       \ Start Cluster       1 @+       \ Low word file length       1 @+ >R    \ High word file length       BASE @ >R HEX           \ Like my numbers in hex       CLSTR/MOD 4 U.R  4 U.R  \ Clusters and bytes       BSWAP 4 U.R             \ First cluster       3 U.R                   \ Attributes       R> BASE !       R> 2*                   \ Back to byte addressing     ELSE       20 +   \ Next entry     THEN     NEXT DROP ;: WRFAT ( - )  \ Write FATs back to disk \ Writing needs to build both FATs, so it is a little \  more work than reading.   DOS0   FAT 0 BLOCK            \ Get first 2 clusters     200 + UNSMASH UPDATE \ Put first part of first FAT   1 BLOCK FAT OVER       \ Get next two clusters     200 + UNSMASH        \ Remainder of first    FAT 100 + SWAP UNSMASH UPDATE  \ First part of second FAT   FAT 100 +   2 BLOCK UNSMASH UPDATE  \ Remainder of second FAT   FLUSH ;CREATE CLUSTER 200 ALLOT ( 1024 Bytes ) \ Cluster buffer: DOSBLOCK ( CLSTR# - ADDR )  \ Needs offset of 4K     4 + BLOCK ;: RDCLUSTER ( Clstr# - ) \ Read cluster from disk to CLUSTER buffer   DOS0   DOSBLOCK                    \ Read disk, one cluster 1K   DUP CLUSTER SMASH           \ First half   200 + CLUSTER 100 + SMASH ; \ Second half: WRCLUSTER ( Clstr# - ) \ Complement of read is write   DOS0   DOSBLOCK CLUSTER OVER UNSMASH    \ First half   CLUSTER 100 + SWAP 200 + UNSMASH \ Second half   UPDATE FLUSH ;: FAT! ( CLSTR# CLSTRADDR - )\ Set FAT entry to point to cluster address. Notice the\  similarity to FAT@. This is used to make the linked list of clusters.   2/MOD 3 * 1 +     \ Get byte offset   FAT 2* + >R       \ Make a byte address and save.   IF                \ Odd entry     DUP 2 TIMES 2*  \ Left shift by 4     0F0 AND         \ Mask unwanted parts     I C@            \ Get what was in FAT to combine     0F AND OR       \ Make one byte     I C!            \ Save it back to FAT     2 TIMES 2/      \ Shift right by 4 to do remaining 8 bits     0FF AND         \ Mask any extra     R> 1 + C!       \ Save it into FAT   ELSE              \ Even entry     DUP 0FF AND     \ Mask 8 bits of 12     I 1 - C!        \ Store in FAT     BSWAP 0F AND    \ Fast shift right by 8 with mask     I C@            \ Fetch other 4 bits     0F0 AND OR      \ Combine together     R> C!           \ Save to FAT   THEN ;: FATEND ( CLSTR# - LSTCLSTR# )\ Follow file clusters to end of file   BEGIN     DUP FAT@     \ Look at each cluster pointer     DUP 0FF8 AND \ 0FF8 thru 0FFF     0FF8 -          WHILE          \ While not end mark     SWAP DROP    \ Follow chain of clusters   REPEAT DROP ;: FATAVAIL ( - FirstAlailableFat )\ Used to find a free FAT entry  1             \ First two are always used  BEGIN    1 +         \ Next FAT    DUP FAT@ 0= \ Unused?  UNTIL ;CREATE FILE0 0C ALLOT    \ First file handleCREATE FILE1 0C ALLOT    \ Second file handleCREATE CRNTFILE  FILE0 , \ Current file opened ( default is FILE0 )\ Changing the current file handle with   FILE1 CRNTFILE !: FLPART ( offset - | - addr )\ Makes handle structure    CREATE            \ Make name     ,                \ Save offset value    DOES R> 7FFF AND  \ Get parm field     @ CRNTFILE @ + ; \ Make data field address\ following is the file handle data structure 00 FLPART NAME      \ Filename 05 FLPART EXT       \ Filename extention 07 FLPART 0CLSTR    \ First cluster 08 FLPART CLSTRS    \ Clusters per file 09 FLPART REMBYTS   \ Bytes in last cluster 0A FLPART LSTCLSTR  \ Last cluster 0B FLPART DIR#      \ Directory offset count: ?NAME ( - )\ Get filename. Sorry this is a long one, but it had a lot\  to do. Parses extension out and puts strings into the\  current file structure defined above.   NAME 7 2020 FILL  \ BL's to name and extension   BL WORD           \ Get string from input   NAME 2* 0 OVER C! \ First byte set to 0 count   0A + 0 SWAP C!    \ Same for extension  2* 1 +  7 FOR         \ Start looking for "." or BL    DUP C@ DUP    20 =    SWAP 2E = OR    IF              \ If either      R> DROP 0 >R  \ Do a leave of FOR NEXT    ELSE            \ Else put string into NAME      DUP C@      NAME 2*      DUP C@       \ Fetch count      1 + OVER C!  \ Increment count      8 + I - C!   \ Store character ( notice trick to index backwards )      1 +          \ Increment string pointer    THEN  NEXT               DUP C@ 2E =      \ If "." then more for extension  IF   1 +   2 FOR           \ Up to 3 bytes     DUP C@ BL =   \ Look for end     IF       R> DROP 0 >R  \ If end of string leave loop     ELSE       DUP C@       EXT 2*       DUP C@ 1+ OVER C! \ Increment string count       3 + I - C! 1 +    \ Save character     THEN   NEXT  THEN DROP ;\ Get a particular files part from directory table: DIRENT ( Dir# Index - cAddr )\ used to calc location of particular directory entry   >R 3 TIMES 2*                 \ Shift by 5 or 020H times   [ DRCT 2 * ] LITERAL + R> + ; \ Returns byte address: DNAME ( Dir# - cAddr )  0 DIRENT ;  \ Name address: DEXT ( Dir# - cAddr ) 8 DIRENT ;    \ Extension address: DATT ( Dir# - cAddr ) 0B DIRENT ;   \ Attribute address: DSTART ( Dir# - cAddr ) 1A DIRENT ;  \ Sector start address: DSIZE ( Dir# - cAddr ) 1C DIRENT ;   \ File size address: DSTART@ ( Dir# - Start ) DSTART 2/ @ BSWAP ; \ Cluster start: DSIZE@ ( Dir# - #Clstr Rem )   DSIZE 2/ 1 @+ @ CLSTR/MOD ;   \ Clusters and remainder bytes.: DTIME ( Dir# - cAddr ) 16 DIRENT ; \ Time string: DDATE ( Dir# - cAddr ) 18 DIRENT ; \ Date string: $COMP ( cAddr1$ cAddr2$ Cnt - Flag )\ String compare to match filenames with input strings\ true = same    false = different  DUP  IF    -1 >R          \ Flag on return stack    1 - FOR        \ Number of bytes to compare.      C@+      SWAP ROT C@+      SWAP ROT -   \ fetch and compare bytes ( faster than = on NC4000 )      IF           \ Not the same?        R> DROP    \ Drop loop count        R> DROP    \ Drop flag       0 >R  0 >R  \  and leave loop on NEXT      THEN    NEXT DROP DROP \ Discard string addresses    R>             \ Return flag  ELSE             \ no length so false    DROP DROP DROP 0  THEN ;: DIRFIND ( - -1=no 0-#DIRS=yes )\ find a name in the DOS directory. Assumes a directory was read\  by RDDIR with something like OPEN  ?NAME   -1  #DIRS FOR             \ Look at one at a time    NAME 2* 1 +         \ Counted string fetched by ?NAME    #DIRS I - DNAME     \ Start from the bottom of the directory    8 $COMP             \ Compare all 8 bytes    IF      EXT 2* 1 +   \ Extension fetched by ?NAME      #DIRS I - DEXT   \ Again from bottom of directory      3 $COMP          \ If both then done      IF       DROP #DIRS R> - \ Calc directory entry #       0 >R            \ Set NEXT count to zero for leave      THEN    THEN  NEXT ;: OPEN  ( - | Filename )\ open specified filename   RDDIR      \ Read directory   RDFAT      \ Read FAT    DIRFIND    \ Find file   DUP 1 +    \ Found?   IF     \ found so read handle info     DUP DSTART@ 0CLSTR !  \ First cluster     DUP DSIZE@ CLSTRS !   \ Number of full clusters     REMBYTS !             \ Remaining bytes in last cluster     0CLSTR @ DUP          \ More than 0 bytes?     IF                          FATEND               \ Then scan for last     THEN     LSTCLSTR !            \ Save last     DIR# !                \ Save directory number   ELSE    \ not found     DROP ABORT" No file? "   THEN ;HEXVARIABLE LINES  0 LINES !: CR+   ( - ) \ Pause every 16 lines and check for quit by ESC key   LINES @ 15 >    IF KEY 1B =      IF QUIT THEN     0 LINES !   ELSE     1 LINES +!   THEN CR ;: EMITTEXT ( ADDR CNT - )\ Simple text typer with pause   FOR     1 @+ SWAP DUP 400D =  \ Note: 4000 part of cmForth     IF DROP CR+           \ Needs to look for CR     ELSE DUP 400A =       \ Ignores LF's       IF DROP       ELSE EMIT           \ Emits characters       THEN     THEN   NEXT DROP ;: FTYPE ( - ) \ Type a specified file as text  OPEN       \ Open file  CR+        \ Do cr  CLSTRS @ ?DUP  \ Check number of clusters  IF    0CLSTR @ SWAP 1 -    FOR              \ For each cluster      DUP DOSBLOCK   \ Read cluster      3FF EMITTEXT   \  and type it      FAT@           \ Get next cluster    NEXT  ELSE    0CLSTR @         \ Just the first one  THEN    REMBYTS @        \ Bytes to type    ?DUP    IF      SWAP DOSBLOCK SWAP 1 -  \ Type only bytes of file      EMITTEXT                \  that remain   THEN   DROP   LINES ! EMPTY-BUFFERS ;DECIMAL: CMOVE ( cFrom cTo Cnt - ) \ not the fastest, but works   1 -   FOR     OVER I + C@     OVER I + C!   NEXT DROP DROP ;: ADD-SIZE ( Bytes - )\ Adds amount of bytes to file length to open file  0CLSTR @ 0=  IF         \ Check for 0 length file    FATAVAIL DUP 0CLSTR ! \ Assign a cluster    LSTCLSTR !            \ Also last  THEN  REMBYTS @ +     \ Add to remaining  1024 /MOD ?DUP  \ Needs more clusters?  IF 1 -    FOR         \ Assign each cluster      FATAVAIL      DUP LSTCLSTR @ FAT!    \ Update FAT      LSTCLSTR ! 1 CLSTRS +! \ Update end    NEXT    -1 LSTCLSTR @ FAT!       \ Mark last in FAT  THEN    REMBYTS ! ;              \ Update remaining bytes( : SUB-SIZE ( BYTES - )\ Reduce file size. Never checked out!( REMBYTS @ SWAP - DUP 0<                          )( IF NEGATE 400 /MOD ?DUP                          )(  IF 1 - FOR CLSTRS @ IF 0CLSTR @ 0 OVER FAT! >R    )(   0CLSTR @ BEGIN DUP FAT@ I - WHILE FAT@ REPEAT R>DROP )(   LSTCLSTR ! -1 CLSTRS ! THEN NEXT 1024 SWAP - 1023 AND )( THEN REMBYTS ! ;      ): DIRAVAIL ( - Dir# )\ scan for a free directory entry to use. Doesn't check to\ see if none is available. For safety, needs check added!  0  BEGIN    DUP DNAME C@    DUP E5 = NOT AND  \ E5 or 0 indicates free  WHILE    1 +  REPEAT ;HEX: UPDATE-DIR ( - )\ Puts new file directory info into directory structure.\ Doesn't do date and time. If you have real-time clock,\  it would be nice to add but isn't truely needed for\  most things.  DIR# @   DUP >R DNAME 2/    \ Word address  DUP 6 2020 FILL    \ File with blanks initially  6 + 0A 0 FILL      \ Other inits  NAME 2* DUP C@  SWAP 1 + I DNAME  ROT CMOVE          \ Put name into dir structure  EXT 2* DUP C@  SWAP 1 + I DEXT  ROT CMOVE         \ Put extension into dir structure  0CLSTR @ BSWAP I DSTART 2/ ! \ First cluster  CLSTRS @ DUP 400 * REMBYTS @ + \ Any remaining  BSWAP R> DSIZE 2/ 1 !+         \ Create length  SWAP 40 / BSWAP SWAP ! ;       \  and store it: WRDIR# ( Dir# - )  DOS0  10 / DUP 100 * DRCT +  SWAP 5 + 2/MOD      \ Calculates the block containing Dir#  BLOCK SWAP 200 * +  UNSMASH            \ Transfers from RAM image to block  UPDATE FLUSH ;     \ Writes back to disk: CREATEFILE ( - | Filename )\ Used to create a file  RDDIR RDFAT        \ Get disk info  ?NAME              \ Fetch a name  DIRAVAIL >R        \ Find directory space  0 CLSTRS !         \ Init clusters  0 REMBYTS !        \  and remaining  I DIR# !           \ What dir entry  0 0CLSTR !         \ First cluster not assigned yet  0 LSTCLSTR !       \ Same with last  UPDATE-DIR         \ Put all this into RAM image of Dir  R> WRDIR# ;        \ Save it to disk: BLK>FILE ( Blk# - )\ Appends block to opened file. Assumes block is on\  hard disk DR2 and DOS disk is 360K floppy DR0. \  Also assumes DOS file is a block file.  FLUSH           \ Empty any offending blocks  400 ADD-SIZE    \ Increase file by 1024 bytes  LSTCLSTR @      \ Find the last cluster  DR2 SWAP BLOCK  \ Fetch from hard disk  DROP DR0        \ Want to flush to DOS disk  4 + 10 @ 12 + !  \ Changes block number to be correct                   \  for DOS disk. This is very implementation-                   \  dependent. One could get the DOSBLOCK into                   \  RAM and do a MOVE but this is faster.  UPDATE           \ Mark block for return to disk  UPDATE-DIR       \ Update directory info  WRFAT            \ Write new FAT  DIR# @ WRDIR# DR2 ; \ Write new directory info\ Many other DOS functions could be added. The basic stuff\  is all here. Things like COPY, RENAME, DELETE, etc.