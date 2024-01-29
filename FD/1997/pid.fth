\
\ pid.fth   A PID controller in Forth

\ This is an ANS Forth program requiring:
\         1. The File wordset
\         2. The Forth Scientific Library ASCII file I/O words
\         3. The Forth Scientific Library random number
\            generator R250 (algorithm #23)

\ This code is released to the public domain  November 1996
\ Taygeta Scientific Inc.

\ $Author:   skip  $
\ $Workfile:   pid.fth  $
\ $Revision:   1.1  $
\ $Date:   11 Dec 1996 08:29:34  $
\ =================Support code for the simulation===================



: d* ;               \ compilation stub for R250, not used anywhere
: umd* ;             \ ditto
: umd/mod ;          \ ditto


S" /usr/local/lib/forth/fsl-util.fth" INCLUDED
S" /usr/local/lib/forth/fsl/r250.seq" INCLUDED
S" /usr/local/lib/forth/fileio.fth"   INCLUDED

-1 VALUE fout             \ output file handle

9 CONSTANT tab_char       \ the TAB character

CREATE crlf   2 ALLOT
CREATE tab    1 ALLOT

: print_endline ( fout -- )
    crlf
    1              \ for MS-DOS use 2 instead of 1
    ROT write-token
;

: print_tab ( fout -- )
    tab
    1
    ROT write-token
;


8 VALUE noise_range        \ the total range of the noise
                           \ set to zero for no noise

100 VALUE max_samples      \ number of samples per run

VARIABLE #samples          \ the number of samples obtained
VARIABLE setting           \ the current sensor input value
VARIABLE tcount            \ the "time" count

TRUE VALUE sensor_once?

: sensor_init ( -- )
    0 #samples !

    sensor_once? IF     \ stuff to do only once
	         1. r250d_init
                -1  tcount !
                 0  setting !
	    THEN
	
    FALSE TO sensor_once?
;


: noise+ ( -- x ) \ get noise sample +- noise_range/2

        noise_range 1 > IF
	                   r250d D>S noise_range MOD
			   noise_range 2/ -
		       ELSE
			   0
		       THEN

;

\ the simulated (noisy) sensor
: sensor@ ( -- x flag )        \ flag is FALSE if no more data

    1 #samples +!
    #samples @ max_samples > IF
	                         0 FALSE
                             ELSE
	                         noise+ setting +!		     
				 setting @ TRUE
			         1 tcount +!   \ increment the "time"
                             THEN
;

\ the simulated external control system output
: controller! ( x -- )

    setting !

    tcount @ fout write-int
             fout print_tab

    setting @ fout write-int
              fout print_endline
;



\ ===================The PID demo code===============================


 32000 CONSTANT ulimit
-32000 CONSTANT llimit

   128 CONSTANT scale_factor

VARIABLE kp            \ proportional gain
VARIABLE ki            \ integral gain
VARIABLE kd            \ differential gain

VARIABLE isum          \ integral sum
VARIABLE old_err       \ previous error value

VARIABLE set_point     \ the INPUT control setting

TRUE VALUE pid_once?

: initialize ( -- )

    10 crlf   C!  13  crlf 1+ C!
    tab_char tab C!

    pid_once? IF
                0 isum !
                0 old_err !
	    THEN

    FALSE TO pid_once?
	
    sensor_init
;

: gains! ( kp ki kd -- )
    kd !
    ki !
    kp !
;

: integrate ( err -- sum )

    isum @ +         \ accumulate the sum

    ulimit MIN       \ apply limits
    llimit MAX

    DUP isum !
;

: differentiate ( err -- diff )

    DUP old_err @ -

    SWAP old_err !
;


\ an integer PID controller, using a scaling factor
: pid ( xin -- xout )

    \ calculate the current error
    set_point @ SWAP -             ( error )

    DUP kp @ scale_factor */                     ( error p )

    OVER integrate ki @ scale_factor */ +        ( error pi )

    SWAP differentiate kd @ scale_factor */ +    ( pid )

;




\ Note: set the gains, e.g.:  64 32 10 gains!
\       BEFORE running pid-run

: pid-run ( set --<outfile>-- )      \ give the setpoint as input

    BL WORD COUNT       \ get the output file name

    \ open the output file
    W/O CREATE-FILE ABORT" unable to open output file" TO fout

    CR

    set_point !

    initialize

    BEGIN
	sensor@ 	\ get the sensor data
    WHILE
	pid             \ apply PID
        controller!     \ set controller
    REPEAT

    fout CLOSE-FILE DROP

;





