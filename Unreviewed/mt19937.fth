\ File: $RCSfile$
\
\ Revision: $Revision$
\ Revision date: $Date$
\ Revision Author: $Author$
\ Author: Skip Carter
\ Creation Date:   1 May 2020
\
\ ====================================================================
\
\ Copyright (c) $Date 1 May 2020$,
\ Taygeta Scientific Incorporated, all rights reserved.
\ This copyright notice does not evidence any actual or intended
\ publication.  This is unpublished computer software containing
\ trade secrets and confidential information proprietary to Taygeta
\ Scientific.
\
\ ====================================================================
\
\
\ Description:  Mersenne Twister  MT19937 random number generator, 32 bit
\               https://en.wikipedia.org/wiki/Mersenne_Twister
\               https://create.stephan-brumme.com/mersenne-twister/
\               ANS compliant
\
\
\ Requires:  FSL fsl-utils.fth
\
\
\ Comments:
\ first 10 values for seed = 5489
\ if treated as uint:
\         3499211612 (d091bb5c)
\         581869302 (22ae9ef6)
\         3890346734 (e7e1faee)
\         3586334585 (d5c31f79)
\         545404204 (2082352c)
\         4161255391 (f807b7df)
\         3922919429 (e9d30005)
\         949333985 (3895afe1)
\         2715962298 (a1e24bba)
\         1323567403 (4ee4092b)
\
\ ====================================================================
\
\
\ $Log$
\
\ ====================================================================

32  constant mt::w
624 constant mt::n
397 constant mt::m
31  constant mt::r
0x9908B0DF constant mt::a 
11 constant mt::u
0x0FFFFFFFF constant mt::d
7 constant mt::s
0x09D2C5680 constant mt::b
15 constant mt::t
0x0EFC60000 constant mt::c
18 constant mt::l
1812433253 constant mt::f
0x0ffff constant mt::wmask

: mt::limit ( v -- v' )

      0x0ffffffff and
;

1 mt::r lshift 1 - mt::limit constant mt::lower_mask
\ mt::lower_mask invert mt::wmask and mt::limit constant mt::upper_mask
0x80000000 constant mt::upper_mask

VARIABLE mt::index

mt::n INTEGER ARRAY mt::buffer{

: mt::dump  ( n -- )                     \ debugging code
     0 do
               decimal i . mt::buffer{ i } @  hex . cr
	loop

;

: mt::twist ( -- )

    \ ." upper and lower masks " hex mt::upper_mask . mt::lower_mask . cr
    mt::n 0 do
	        mt::buffer{ i } @ mt::upper_mask and
	        mt::buffer{ i 1+ mt::n mod } @ mt::lower_mask and
	        +                                           ( x )
	        dup
	        1 rshift  swap                              ( xa x )
	        2 mod
	        if mt::a xor then
		mt::buffer{ i mt::m + mt::n mod } @ xor
		mt::buffer{ i } !
     loop

     0 mt::index !
;

: mt::seed ( s -- )

    mt::buffer{ 0 } !
    mt::n 1 do
	        mt::buffer{ i 1 - } @  dup         ( buf[i-1] buf[i-1] )
	        mt::w 2 - rshift
	        xor mt::f * i +
	        mt::limit mt::buffer{ i } !
    loop

    mt::twist

;

: mt::rand (  -- x )

    mt::index @ mt::n >=
    if
	mt::index @ mt::n >
	if                     \ was never seeded
           4589 mt::seed
	then

	mt::twist
	
    then

    
    mt::index 
    mt::buffer{ over @  } @     ( 'index buffer[index]  )
    swap 1 swap +!

    dup mt::u rshift   mt::d and  xor 0x0ffffffff and
    dup mt::s lshift   mt::b and  xor 0x0ffffffff and
    dup mt::t lshift   mt::c and  xor 0x0ffffffff and
    dup mt::l rshift xor   0x0ffffffff and
    

;


TEST-CODE? [IF]     \ test code =============================================

10 INTEGER ARRAY ref{
    
: init-refs (  )

    0xd091bb5c ref{ 0 } !
    0x22ae9ef6 ref{ 1 } !
    0xe7e1faee ref{ 2 } !
    0xd5c31f79 ref{ 3 } !
    0x2082352c ref{ 4 } !
    0xf807b7df ref{ 5 } !
    0xe9d30005 ref{ 6 } !
    0x3895afe1 ref{ 7 } !
    0xa1e24bba ref{ 8 } !
    0x4ee4092b ref{ 9 } !

;

\ shows the first 10 values and the reference values for seed 5489
: mt_test (  )

    init-refs
    
    5489 mt::seed

    
     10 0 do
	 mt::rand
	 dup decimal u. hex u.
	 ref{ i } @ u. cr

     loop
;


[THEN]
