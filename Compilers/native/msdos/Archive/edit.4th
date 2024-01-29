
\ Norton Editor Word				rdb 11/29/86

\ This word allows the use of the Norton Editor (or any other
\ by simply changing the name) from within UR/FORTH.
\
\ Date:		Who/Rev:	Comment:
\ 11/29/86	rdb/100		In the beginning ...

: NE	( -- )
	" NE "		COUNT	\ Your favorite editor
	BL WORD		COUNT	\ Allow command line switches
	STRCAT STRPCK ~SHELL ;	\ Call COMMAND.COM


