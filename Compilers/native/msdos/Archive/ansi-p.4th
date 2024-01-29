\ ANSI.4TH					rdb 11/29/86
\
\ This file contain the DEFERed words and the STATE structure
\ needed to emulate a Digital Equipment Corp. VT-100 terminal in
\ the ANSI mode. The word names correspond to the DEC and ANSI
\ X3.64 mnemonics (such as they are) for their functions. As
\ mnemonics, they're not. Words invoked with default values are
\ indicated by _d name extensions.
\
\ Some numeric arguments are specified with a leading ?.  The
\ driver converts such numbers to their negative values, while
\ the resolution actions for the DEFERed words should output a ?
\ if their arguments are negative, followed by the absolute
\ value of the numeric arguments.
\
\ Several of the words have a variable number of arguments
\ (determined at runtime) on the stack. This situation can be
\ handled by the following code fragment (providing the only
\ values on the stack are arguments for the word).
\
\      depth 1- 0              \ #args-1 on stack
\ do   dup 0<                  \ Need a "?"?
\  if  ascii ? emit            \ There you go
\  then abs .  ascii ; emit    \ Emit abs value, then a ;
\ loop abs .                   \ Last u
\
\ Words taking multiple arguments from the stack, separated by
\ a ";", assume that the only values on the stack are arguments
\ for the functiron.  Unpredictable results will occur if extra
\ arguments are on the stack at the start of the function.
\
\ Date:		Who/Rev:	Comments:
\ 11/29/86	rdb/104		Moved over to UR/FORTH

\ **************************************************************
\ DEC VT-100 (ANSI X3.64) ESCape Sequences:
\ **************************************************************

VOCABULARY ANSI_VOC   
ANSI_VOC DEFINITIONS   			\ Put words in ANSI_VOC

DEFER DECSC    \ esc 7         Save cursor position, wrap flag,
               \               attribute, char sets, and origin
               \               mode status.
DEFER DECRC    \ esc 8         Restore status saved by decsc.
DEFER DECKPAM  \ esc =         Turn on keypad application mode.
DEFER DECKPNM  \ esc >         Turn off keypad application mode.
DEFER RIS      \ esc c         Reinitialize terminal; when XON
               \               handshaking is active, an XON is
               \               sent on completion.
DEFER IND      \ esc D         Move cursor down one row, scroll
               \               screen up at last row.
DEFER NEL      \ esc E         Move cursor to far left of next
               \               row, scroll screen at last row.
DEFER HTS      \ esc H         Set tab stop at current position.
DEFER RI       \ esc M         Move cursor up one row, scroll
               \               screen down at first row.
DEFER DECID    \ esc Z         Identify terminal.
DEFER DECSHL   \ esc # 3       Enable double-height chars.
DEFER DECDHL   \ esc # 4       Disable double-height chars.
DEFER DECSWL   \ esc # 5       Enable double-wide chars
DEFER DECDWL   \ esc # 6       Disable double-wide chars.
DEFER DECALN   \ esc # 8       Display screen alignment pattern.
DEFER SCS_(0   \ esc ( 0       Change G0 char set to graphics.
DEFER SCS_(A   \ esc ( A       Change G0 char set to UK set
DEFER SCS_(B   \ esc ( B       Change G0 char set to US ASCII
DEFER SCS_)0   \ esc ) 0       Change G1 char set to graphics
DEFER SCS_)A   \ esc ) A       Change G1 char set to UK set.
DEFER SCS_)B   \ esc ) B       Change G1 char set to US ASCII
DEFER DECSTR   \ esc ! p       Reset all terminal modes; when
               \               XON is active, an XON is sent
DEFER ICH_D    \ esc [ @       Insert 1 blank char at cursor.
DEFER CUU_D    \ esc [ A       Move cursor up 1 row.
DEFER CUD_D    \ esc [ B       Move cursor down 1 row.
DEFER CUF_D    \ esc [ C       Move cursor right 1 column.
DEFER CUB_D    \ esc [ D       Move cursor left 1 column.
DEFER CNL_D    \ esc [ E       Move cursor down 1 row to col 1.
DEFER CPL_D    \ esc [ F       Move cursor up 1 row to col 1.
DEFER HVP_D    \ esc [ f       Move cursor to row 1, col 1.
DEFER CHA_D    \ esc [ G       Move cursor to col 1.
DEFER TBC_D    \ esc [ g       Clear tab stop at cursor col.
DEFER CUP_D    \ esc [ H       Move cursor to row 1, col 1.
DEFER CHT_D    \ esc [ I       Tab cursor forward 1 tab stops.
DEFER MC_D     \ esc [ i       Copy screen to aux (printer) port
DEFER ED_D     \ esc [ J       Erase screen from cursor to end.
DEFER EL_D     \ esc [ K       Erase row from cursor to end.
DEFER IL_D     \ esc [ L       Insert 1 blank row at cur row.
DEFER DS_D     \ esc [ M       Delete cursor row.
DEFER SGR_D    \ esc [ m       Set normal attributes
DEFER DSR_D    \ esc [ n       Send status (esc [ 0 if ready).
DEFER DCH_D    \ esc [ P       Delete 1 char at cursor col.
DEFER DECLL_D  \ esc [ q       Turn off simulated LEDs L1 to L4.
DEFER DECSTBM_D \ esc [ r      Set scroll region to rows 1-24.
DEFER ECH_D    \ esc [ X       Erase 1 char at cursor col.
DEFER CBT_D    \ esc [ Z       Tab cursor backword 1 tab stop.
DEFER DECSED_D \ esc [ ? J     Erase nomal enhanced data from
               \               cursor to end of screen.
DEFER DECSEL_D \ esc [ ? K     Erase normal enhanced data from
               \               cursor to end of line.
\     DSR_D    \ esc ? n       Report printer status.
DEFER ICH      \ esc [ nn @    Insert nn blank chars at cursor.
DEFER CUU      \ esc [ nn A    Move cursor up nn rows
DEFER CUD      \ esc [ nn B    Move cursor down nn rows.
DEFER CUF      \ esc [ nn C    Move cursor right nn columns.
DEFER DA       \ esc [ 0 c     Transmit active terminal attribs.
DEFER CUB      \ esc [ nn D    Move cursor left nn columns.
DEFER CNL      \ esc [ nn E    Move cursor down nn rows to col 1
DEFER CPL      \ esc [ nn F    Move cursor up nn rows to col 1
DEFER CHA      \ esc [ nn G    Move cursor to column nn.
DEFER TBC      \ esc [ nn g    0 = Clear tab stop at cursor col
               \               3 = Clear all tab stops.
DEFER CHT      \ esc [ nn I    Tab forward nn tab stops.
DEFER MC       \ esc [ nn i    0 = Copy screen to aux (printer)
               \               2 = Copy screen to modem (host)
               \               4 = Disable transparent passthru
               \               5 = Enable transparent passthru
DEFER ED       \ esc [ nn J    0 = Erase screen cursor to end
               \               1 = Erase screen beginning to cur
               \               2 = Erase screen
DEFER EL       \ esc [ nn K    0 = Erase row cursor to end
               \               1 = Erase row beginning to cur
               \               2 = Erase row
DEFER IL       \ esc [ nn L    Insert nn blank rows at cur row.
DEFER DS       \ esc [ nn M    Delete nn rows at cursor row
DEFER SGR      \ esc [ nn m    0 = Set video attribs off
               \               1 = Set high intensity video
               \               4 = Set undeline video
               \               5 = Set blinking video
               \               7 = Set reverse video
DEFER DSR      \ esc [ nn n    5 = Send status esc [ 0 if ready
               \               6 = Report cur pos esc [ x ; y R
DEFER DCH      \ esc [ nn P    Delete nn chars at cursor col
DEFER DECLL    \ esc [ nn q    0 = Turn simultaed LEDs L1-L4 off
               \               1 = L1 on
               \               2 = L2 on
               \               3 = L3 on
               \               4 = L4 on
DEFER ECH      \ esc [ nn X    Erase nn chars cur cursor col
DEFER CBT      \ esc [ nn Z    Tab cursor backword nn tab stops
\     MC       \ esc [ ? nn i  1 = Copy cursor row to aux port
               \               3 = Copy cursor row to modem port
               \               4 = Disable copy passthru print
               \               5 = Enable copy passthru print
DEFER DECSED   \ esc [ ? nn J  0 = Erase normal enhance data
               \                   from cursor to end of screen
               \               1 = Erase normal enhanced data
               \                   from beginning of scr to cur
               \               2 = Erase normal enhanced data
               \                   from screen
\     DSR      \ esc [ ? nn n  15 = Report printer status
               \               Ready         = esc [ ? 10 n
               \               Not ready     = esc [ ? 11 n
DEFER DECSEL   \ esc [ ? nn K  0 = Erase normal enhance data
               \                   from cursor to end of line
               \               1 = Erase normal enhanced data
               \                   from beginning of line to cur
               \               2 = Erase normal enhanced data
               \                   from row
DEFER HVP      \ esc [ n1 ; n2 f Move cur to row n1, col n2
DEFER CUP      \ esc [ n1 ; n2 H Move cur to row n1, col n2
DEFER DECSTBM  \ esc [ n1 ; n2 r Set scroll region to rows n1-n2
DEFER SM       \ esc [ n1 ; ... ; nx h Turn on terminal modes
DEFER RM       \ esc [ n1 ; ... ; nx l Turn off terminal modes
               \ Terminal modes:
               \ 1 = Transfer enhanced data mode
               \ 2 = Keyboard lock mode.
               \ 3 = Monitor mode.
               \ 4 = Insert char mode.
               \ 6 = Clear enhanced data mode
               \ 12 = Local echo disable mode.
               \ 13 = Control execution disable mode.
               \ 16 = Cursor transfer termination mode.
               \ ?1 = Cursor key mode.
               \ ?2 = ANSI mode (ATS mode when off)
               \ ?3 = 132 column mode
               \ ?4 = Smooth scroll mode.
               \ ?5 = Reverse screen mode
               \ ?6 = Origin mode.
               \ ?7 = Character wrap mode.
               \ ?8 = Auto repeat mode.
               \ ?10 = Block mode.
               \ ?18 = Print form feed mode.
               \ ?19 = Print full screen mode.
               \ ?25 = Enable cursor mode.

\ A ? preceding a value causes all following values to be
\ treated as though they had preceding "?".

: ERROR        ( -- )          \ Error handler for ANSI_STATE
        DROP CR ."  Error in terminal escape sequence" CR  ;
 
: N     ( char -- n )          48 -  ;

: -N    ( char -- -n )         48 - NEGATE  ;

: N10*+ ( n1 char -- n2 )      N SWAP 10 * +  ;

: -N10*+ ( -n1 char -- -n2 )   -N SWAP 10 * +  ;

127 CONSTANT AMASK

\ State table definition:
\ 10  9  8  7  6  5  4  3  2  1  0  #
  15 11 14 37 34  1  3  3  5 14  2 11 FSM ANSI_FSM

\ efa  nstate  match    mask    state term# >fsm ansi_fsm
 
' DROP      1  27       AMASK   0       0   >FSM ANSI_FSM
' NOOP      0  0      	0       0       1   >FSM ANSI_FSM
 
' DECSC     0  ASCII 7	AMASK 	1       2   >FSM ANSI_FSM
' DECRC     0  ASCII 8	AMASK   1       3   >FSM ANSI_FSM
' DECKPAM   0  ASCII =	AMASK   1       4   >FSM ANSI_FSM
' DECKPNM   0  ASCII >	AMASK   1       5   >FSM ANSI_FSM
' RIS       0  ASCII c	AMASK   1       6   >FSM ANSI_FSM
' IND       0  ASCII D 	AMASK   1       7   >FSM ANSI_FSM
' NEL       0  ASCII E  AMASK   1       8   >FSM ANSI_FSM
' HTS       0  ASCII H  AMASK   1       9   >FSM ANSI_FSM
' RI        0  ASCII M  AMASK   1       10  >FSM ANSI_FSM
' DECID     0  ASCII Z  AMASK   1       11  >FSM ANSI_FSM
' DROP      2  ASCII #  AMASK   1       12  >FSM ANSI_FSM
' DROP      3  ASCII (  AMASK   1       13  >FSM ANSI_FSM
' DROP      4  ASCII )  AMASK   1       14  >FSM ANSI_FSM
' DROP      5  ASCII !  AMASK   1       15  >FSM ANSI_FSM

' DECSHL    0  ASCII 3  AMASK   2       16  >FSM ANSI_FSM
' DECDHL    0  ASCII 4  AMASK   2       17  >FSM ANSI_FSM
' DECSWL    0  ASCII 5  AMASK   2       18  >FSM ANSI_FSM
' DECDWL    0  ASCII 6  AMASK   2       19  >FSM ANSI_FSM
' DECALN    0  ASCII 8  AMASK   2       20  >FSM ANSI_FSM

' SCS_(0    0  ASCII 0  AMASK   3       21  >FSM ANSI_FSM
' SCS_(A    0  ASCII A  AMASK   3       22  >FSM ANSI_FSM
' SCS_(B    0  ASCII B  AMASK   3       23  >FSM ANSI_FSM

' SCS_)0    0  ASCII 0  AMASK   4       24  >FSM ANSI_FSM
' SCS_)A    0  ASCII A  AMASK   4       25  >FSM ANSI_FSM
' SCS_)B    0  ASCII B  AMASK   4       26  >FSM ANSI_FSM

' DECSTR    0  ASCII p  AMASK   5       27  >FSM ANSI_FSM

' ICH_D     0  ASCII @  AMASK   6       28  >FSM ANSI_FSM
' CUU_D     0  ASCII A  AMASK   6       29  >FSM ANSI_FSM
' CUD_D     0  ASCII B  AMASK   6       30  >FSM ANSI_FSM
' CUF_D     0  ASCII C  AMASK   6       31  >FSM ANSI_FSM
' CUB_D     0  ASCII D  AMASK   6       32  >FSM ANSI_FSM
' CNL_D     0  ASCII E  AMASK   6       33  >FSM ANSI_FSM
' CPL_D     0  ASCII F  AMASK   6       34  >FSM ANSI_FSM
' HVP_D     0  ASCII f  AMASK   6       35  >FSM ANSI_FSM
' CHA_D     0  ASCII G  AMASK   6       36  >FSM ANSI_FSM
' TBC_D     0  ASCII g  AMASK   6       37  >FSM ANSI_FSM
' CUP_D     0  ASCII H  AMASK   6       38  >FSM ANSI_FSM
' CHT_D     0  ASCII I  AMASK   6       39  >FSM ANSI_FSM
' MC_D      0  ASCII i  AMASK   6       40  >FSM ANSI_FSM
' ED_D      0  ASCII J  AMASK   6       41  >FSM ANSI_FSM
' EL_D      0  ASCII K  AMASK   6       42  >FSM ANSI_FSM
' IL_D      0  ASCII L  AMASK   6       43  >FSM ANSI_FSM
' DS_D      0  ASCII M  AMASK   6       44  >FSM ANSI_FSM
' SGR_D     0  ASCII m  AMASK   6       45  >FSM ANSI_FSM
' DSR_D     0  ASCII n  AMASK   6       46  >FSM ANSI_FSM
' DCH_D     0  ASCII P  AMASK   6       47  >FSM ANSI_FSM
' DECLL_D   0  ASCII q  AMASK   6       48  >FSM ANSI_FSM
' DECSTBM_D 0  ASCII r  AMASK   6       49  >FSM ANSI_FSM
' ECH_D     0  ASCII X  AMASK   6       50  >FSM ANSI_FSM
' CBT_D     0  ASCII Z  AMASK   6       51  >FSM ANSI_FSM
' N         7  ASCII 0  AMASK   6       52  >FSM ANSI_FSM
' N         7  ASCII 1  AMASK   6       53  >FSM ANSI_FSM
' N         7  ASCII 3  AMASK   6       54  >FSM ANSI_FSM
' N         7  ASCII 4  AMASK   6       55  >FSM ANSI_FSM
' N         7  ASCII 5  AMASK   6       56  >FSM ANSI_FSM
' N         7  ASCII 6  AMASK   6       57  >FSM ANSI_FSM
' N         7  ASCII 7  AMASK   6       58  >FSM ANSI_FSM
' N         7  ASCII 8  AMASK   6       59  >FSM ANSI_FSM
' N         7  ASCII 9  AMASK   6       60  >FSM ANSI_FSM
' DROP      8  ASCII ?  AMASK   6       61  >FSM ANSI_FSM

' ICH       0  ASCII @  AMASK   7       62  >FSM ANSI_FSM
' CUU       0  ASCII A  AMASK   7       63  >FSM ANSI_FSM
' CUD       0  ASCII B  AMASK   7       64  >FSM ANSI_FSM
' CUF       0  ASCII C  AMASK   7       65  >FSM ANSI_FSM
' DA        0  ASCII c  AMASK   7       66  >FSM ANSI_FSM
' CUB       0  ASCII D  AMASK   7       67  >FSM ANSI_FSM
' CNL       0  ASCII E  AMASK   7       68  >FSM ANSI_FSM
' CPL       0  ASCII F  AMASK   7       69  >FSM ANSI_FSM
' HVP       0  ASCII f  AMASK   7       70  >FSM ANSI_FSM
' CHA       0  ASCII G  AMASK   7       71  >FSM ANSI_FSM
' TBC       0  ASCII g  AMASK   7       72  >FSM ANSI_FSM
' HVP       0  ASCII H  AMASK   7       73  >FSM ANSI_FSM
' SM        0  ASCII h  AMASK   7       74  >FSM ANSI_FSM
' CHT       0  ASCII I  AMASK   7       75  >FSM ANSI_FSM
' MC        0  ASCII i  AMASK   7       76  >FSM ANSI_FSM
' ED        0  ASCII J  AMASK   7       77  >FSM ANSI_FSM
' EL        0  ASCII K  AMASK   7       78  >FSM ANSI_FSM
' IL        0  ASCII L  AMASK   7       79  >FSM ANSI_FSM
' RM        0  ASCII l  AMASK   7       80  >FSM ANSI_FSM
' DS        0  ASCII M  AMASK   7       81  >FSM ANSI_FSM
' SGR       0  ASCII m  AMASK   7       82  >FSM ANSI_FSM
' DSR       0  ASCII n  AMASK   7       83  >FSM ANSI_FSM
' DCH       0  ASCII P  AMASK   7       84  >FSM ANSI_FSM
' DECLL     0  ASCII q  AMASK   7       85  >FSM ANSI_FSM
' DECSTBM   0  ASCII r  AMASK   7       86  >FSM ANSI_FSM
' ECH       0  ASCII X  AMASK   7       87  >FSM ANSI_FSM
' N10*+     7  ASCII 0  AMASK   7       88  >FSM ANSI_FSM
' N10*+     7  ASCII 1  AMASK   7       89  >FSM ANSI_FSM
' N10*+     7  ASCII 2  AMASK   7       90  >FSM ANSI_FSM
' N10*+     7  ASCII 3  AMASK   7       91  >FSM ANSI_FSM
' N10*+     7  ASCII 4  AMASK   7       92  >FSM ANSI_FSM
' N10*+     7  ASCII 5  AMASK   7       93  >FSM ANSI_FSM
' N10*+     7  ASCII 6  AMASK   7       94  >FSM ANSI_FSM
' N10*+     7  ASCII 7  AMASK   7       95  >FSM ANSI_FSM
' N10*+     7  ASCII 8  AMASK   7       96  >FSM ANSI_FSM
' N10*+     7  ASCII 9  AMASK   7       97  >FSM ANSI_FSM
' DROP      9  ASCII ;  AMASK   7       98  >FSM ANSI_FSM
 
' DECSED_D  0  ASCII J  AMASK   8       99  >FSM ANSI_FSM
' DECSEL_D  0  ASCII K  AMASK   8       100 >FSM ANSI_FSM
' DSR_D     0  ASCII n  AMASK   8       101 >FSM ANSI_FSM
' DROP      8  ASCII ?  AMASK   8       102 >FSM ANSI_FSM
' -N        10 ASCII 0  AMASK   8       103 >FSM ANSI_FSM
' -N        10 ASCII 1  AMASK   8       104 >FSM ANSI_FSM
' -N        10 ASCII 2  AMASK   8       105 >FSM ANSI_FSM
' -N        10 ASCII 3  AMASK   8       106 >FSM ANSI_FSM
' -N        10 ASCII 4  AMASK   8       107 >FSM ANSI_FSM
' -N        10 ASCII 5  AMASK   8       108 >FSM ANSI_FSM
' -N        10 ASCII 6  AMASK   8       109 >FSM ANSI_FSM
' -N        10 ASCII 7  AMASK   8       110 >FSM ANSI_FSM
' -N        10 ASCII 8  AMASK   8       111 >FSM ANSI_FSM
' -N        10 ASCII 9  AMASK   8       112 >FSM ANSI_FSM
 
' N         7  ASCII 0  AMASK   9       113 >FSM ANSI_FSM
' N         7  ASCII 1  AMASK   9       114 >FSM ANSI_FSM
' N         7  ASCII 2  AMASK   9       115 >FSM ANSI_FSM
' N         7  ASCII 3  AMASK   9       116 >FSM ANSI_FSM
' N         7  ASCII 4  AMASK   9       117 >FSM ANSI_FSM
' N         7  ASCII 5  AMASK   9       118 >FSM ANSI_FSM
' N         7  ASCII 6  AMASK   9       119 >FSM ANSI_FSM
' N         7  ASCII 7  AMASK   9       120 >FSM ANSI_FSM
' N         7  ASCII 8  AMASK   9       121 >FSM ANSI_FSM
' N         7  ASCII 9  AMASK   9       122 >FSM ANSI_FSM
' DROP      8  ASCII ?  AMASK   9       123 >FSM ANSI_FSM
 
' MC        0  ASCII i  AMASK   10      124 >FSM ANSI_FSM
' DECSED    0  ASCII J  AMASK   10      125 >FSM ANSI_FSM
' DECSEL    0  ASCII K  AMASK   10      126 >FSM ANSI_FSM
' DSR       0  ASCII n  AMASK   10      127 >FSM ANSI_FSM
' -N10*+    10 ASCII 0  AMASK   10      128 >FSM ANSI_FSM
' -N10*+    10 ASCII 1  AMASK   10      129 >FSM ANSI_FSM
' -N10*+    10 ASCII 2  AMASK   10      130 >FSM ANSI_FSM
' -N10*+    10 ASCII 3  AMASK   10      131 >FSM ANSI_FSM
' -N10*+    10 ASCII 4  AMASK   10      132 >FSM ANSI_FSM
' -N10*+    10 ASCII 5  AMASK   10      133 >FSM ANSI_FSM
' -N10*+    10 ASCII 6  AMASK   10      134 >FSM ANSI_FSM
' -N10*+    10 ASCII 7  AMASK   10      135 >FSM ANSI_FSM
' -N10*+    10 ASCII 8  AMASK   10      136 >FSM ANSI_FSM
' -N10*+    10 ASCII 9  AMASK   10      137 >FSM ANSI_FSM
' DROP      8  ASCII ;  AMASK   10      138 >FSM ANSI_FSM

: ANSI_KEY     ( -- c )
 BEGIN KEY DUP ANSI_FSM
 AGAIN ;
