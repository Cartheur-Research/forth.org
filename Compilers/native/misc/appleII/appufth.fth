(               **** UNIFORTH SAMPLER ****                 )    (     requires Apple // series computer with ProDOS        )                                                                    ( Copyright 1985, Unified Software Systems                 )    (                 1288 Nantucket Ave.                      )    (                 Columbus, OH 43220                       )    ( For technical assistance call 301-776-7609               )                                                                    ( Current FORTH-83 implementation written by L.E. Brotzman )    ( Derived from FIG-FORTH version 1.1 written by R. Kuntze  )    ( and further modified for Commodore 64 by R. Parise       )    ( FIG-FORTH is made available by:                          )    ( FORTH Interest Group                                     )    ( P.O. Box 1105                                            )    ( San Carlos, CA 94070                                     )                                                                    ( Sysgen words -- 1                              02-SEP-85 LEB) DECIMAL FORTH DEFINITIONS  ( set current and context vocabs )   LATEST 12 +ORIGIN ! ( LATEST )  HERE 36 +ORIGIN ! ( FENCE )     HERE 38 +ORIGIN ! ( DP )   VOC-LINK @ 40 +ORIGIN ! ( VOC-LINK ) ' FORTH 2+ @ 14 +ORIGIN ! ( Vocabulary thread )                                                                                 VARIABLE ININAM 66 ALLOT                                        : XX  BL WORD DUP C@ 1+ ROT DUP 66 BLANKS SWAP CMOVE ;          ININAM XX /SAMPLER/FORTH.FTH  FORGET XX ( now that name loaded )                                                                : DEFFILE  ( Resets CHANA to default file )                        CR ." Pathname of file to be opened upon FORTH boot-up"         CR ." /SAMPLER/FORTH.FTH is default if <cr> entered: "          PAD 64 EXPECT  SPAN @ IF ININAM 66 BLANKS                       SPAN @ ININAM C!  PAD ININAM 1+ SPAN @ 64 MIN CMOVE  THEN       ININAM PATHNAME 66 CMOVE ;  -->                              ( Sysgen words -- 2                              02-SEP-85 LEB) HEX                                                             FF CONSTANT FTYP  ( FF is system file; 06 is binary file )                                                                      : (SYSOPEN)  ( open binary file for system )  C8 PDCMD C!          3 PDPARM C!  TEMP.PATHNAME PDPARM 1+ !                          PDIOFCB @ PDPARM 3 + !  MLICALL ;                            : (SYSMAKE)  ( make binary file for system )  C0 PDCMD C!          7 PDPARM C!  TEMP.PATHNAME PDPARM 1+ !  C3 PDPARM 3 + C!        FTYP PDPARM 4 + C!  0 +ORIGIN PDPARM 5 + !  1 PDPARM 7 + C!     DATE @ 0= IF !DATE THEN  DATE @ PDPARM 8 + !                    TIME @ 0= IF !TIME THEN  TIME @ PDPARM A + !                    MLICALL ?DUP IF DUP PDERR ABORT" in (SYSMAKE)" THEN ;        -->                                                                                                                                                                                             ( Sysgen words -- 3                              02-SEP-85 LEB) : .SYSWRITE ( write system to disk )  CB PDCMD C!                  4 PDPARM C!  REFNUM @ PDPARM 1+ C!  0 +ORIGIN PDPARM 2+ !       HERE 1+ 0 +ORIGIN - PDPARM 4 + !                                MLICALL ?DUP IF DUP PDERR ABORT" in .SYSWRITE" THEN ;        : .SYSEOF  ( set EOF to MARK )  CF PDCMD C!                        2 PDPARM C! REFNUM @ PDPARM 1+ C!                               MLICALL ?DUP IF DUP PDERR ABORT" in .SYSEOF" THEN               D0 PDCMD C!                                                     MLICALL ?DUP IF DUP PDERR ABORT" in .SYSEOF" THEN ;          : .SYSOPEN  ( open file for system )                               (SYSOPEN)  DUP 46 = IF  DROP (SYSMAKE) (SYSOPEN) THEN           ?DUP IF DUP PDERR ABORT"  in .SYSOPEN" THEN                     PDPARM 5 + C@ REFNUM ! ;                                     -->                                                                                                                             ( Sysgen words -- 4                             02-SEP-85 LEB ) DECIMAL                                                         : SYSGEN  ( save system image to disk )                            CHANA CLOSE GET-TMP.PTHNAM DEFFILE                              .SYSOPEN .SYSWRITE .SYSEOF CLOSE ;                           : GETVALS  ( input number routine for reallocate )                 CR ." Number of block buffers (2 to n): " GETNUM                CR ." New memory size in Kbytes (30-48): " GETNUM CR ;       : ENDALL  ( print terminating messages for reallocate )            ." End reallocate. SYSGEN or COLD to use new limits." CR ;   : REALLOCATE  ( change initial memory size and no. of buffers )    GETVALS 1024 * 48896 MIN 128 - DUP 24 +ORIGIN !  ( set UP )     OVER B/BUF 6 + * - DUP 42 +ORIGIN !  ( set FIRST )              100 - 30 +ORIGIN !  44 +ORIGIN !  ( set TIB; #BUFF )            ENDALL ;                                                     -->                                                             ( Sysgen words -- 5  SCRAMBLE                   20-SEP-85 LEB ) : (SCRAMBLE)  ( perform the actual erasure of links )              14 +ORIGIN PAD 40 + 8 CMOVE                                     BEGIN  PAD 40 + (LATEST) SWAP ?DUP WHILE                          NAME> >LINK DUP @ ROT ! 0 SWAP !  REPEAT DROP ;            : SCRAMBLE   ( OEM development tool...wipes out links )            ( use this word on your sysgen, and no royalties are)           ( required.  Used like SCRAMBLE /pathname - see SYSGEN )        CR ." SCRAMBLE...Are you sure? " Y/N 0> IF CR                   ." Have you set TURNKEY? " Y/N 0> IF (SCRAMBLE)                 SYSGEN CR ." Finished scrambling...now rebooting!!"             3000 MS  BYE THEN THEN ;                                     ;S                                                                                                                                                                                                                                                              ;S                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              \ Low-resolution graphics support -- 1          05-SEP-85 LEB   \ These routines make direct calls to the monitor firmware      \ routines of the same name.  See the Apple //c Reference       \ Manual, Vol 1, pp. 115-119.                                   HEX                                                             CODE CLREOL      \ --- ...clears text line from cursor to eol    FC9C JSR, NEXT, END-CODE                                       CODE CLEOLZ      \ n --- ...clears line from BASL(=$28)+n        BOT LDY, FC9E JSR, POP, END-CODE                               CODE CLREOP      \ --- ...clears text window from cursor         FC42 JSR, NEXT, END-CODE                                       CODE CLRSCR      \ --- ...clears low-res display                 F832 JSR, NEXT, END-CODE                                       CODE CLRTOP      \ --- ...clears top 40 lines of low-res display F836 JSR, NEXT, END-CODE                                       -->                                                             \ Low-resolution graphics support -- 2          05-SEP-85 LEB   CODE PLOT    \ h v --- ...plot single block in low-res display               \ h=horizontal coordinate; v=vertical coordinate    BOT LDA, SEC LDY, F800 JSR, POPTWO, END-CODE                   CODE HLINE   \ v lh rh --- ...draw horizontal line on low-res                \ v=vert coord; lh=left coord; rh=right coord       BOT LDA, 2C STA, SEC LDY, 4 ,X LDA, F819 JSR,                   INX, INX, POPTWO, END-CODE                                     CODE VLINE   \ h tv bv --- ...draw vertical line on low-res                  \ h=horiz. coord; tv=top coord; bv=bottom coord     BOT LDA, 2D STA, SEC LDA, 4 ,X LDY, F828 JSR,                   INX, INX, POPTWO, END-CODE                                     CODE HOME    \ --- ...clears display--puts cursor at upper-left  FC58 JSR, NEXT, END-CODE                                       -->                                                                                                                             \ Low-resolution graphics support -- 3          05-SEP-85 LEB   CODE SETCOL     \ col --- ...sets color used for low-res         BOT LDA, F864 JSR, POP, END-CODE                               CODE SCRN       \ h v --- col ...read color of block at h & v    SEC LDY, BOT LDA, F871 JSR, SEC STA, 0 # LDA, SEC 1+ STA,       POP, END-CODE                                                  CODE TEXT       \ --- ...set 80 column text mode                 C00D STA, C051 LDA, 0 # LDA, 36 STA, C3 # LDA, 37 STA,          NEXT, END-CODE                                                 CODE LOW-RES    \ --- ...activate low-res graphics mode          C00C STA, C054 STA, C050 STA, C054 STA, C058 STA,               C07E STA, C05F STA, NEXT, END-CODE                             CODE MIXED      \ --- ...activate mixed text & low-res graphics  C00D STA, C054 STA, C050 STA, C053 STA, C058 STA,               C07E STA, C05F STA, NEXT, END-CODE                             DECIMAL ;S                                                      \ GRAPHICS TEST                                                 \ After loading this block just type "TEST" and a carriage      \ return.  Pressing any key will return you to text mode.                                                                       : TEST  LOW-RES  CLRSCR  15 SETCOL                                 2 1 5 VLINE 5 2 3 HLINE 4 1 5 VLINE                   \ 'U'     6 1 5 VLINE 7 2 PLOT 8 3 PLOT 9 4 PLOT 10 1 5 VLINE   \ 'N'     1 12 14 HLINE 13 2 4 VLINE 5 12 14 HLINE              \ 'I'     16 1 5 VLINE 1 17 18 HLINE 17 3 PLOT                  \ 'F'     20 1 5 VLINE 21 1 PLOT 21 5 PLOT 22 1 5 VLINE         \ 'O'     24 1 5 VLINE 25 1 PLOT 25 3 PLOT 26 1 3 VLINE                   25 4 PLOT 26 5 PLOT                                   \ 'R'     1 28 30 HLINE 29 2 5 VLINE                            \ 'T'     32 1 5 VLINE 33 3 PLOT 34 1 5 VLINE                   \ 'H'     KEY DROP TEXT ;                                              ;S                                                              