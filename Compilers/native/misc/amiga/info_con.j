
\ JForth code for the information content of various population distributions
\	James C. Brakefield	April 13, 1991
\	placed in the public domain, written in JForth for Amiga

\	Contains:
\ Rsum		reciprical sum
\ Rgsum		ranged reciprical sum
\ Acode		theoretical info content of Zipf population
\ Ccode		practical encoding info content of Zipf population
\ Info		theoretical info contede? {	ju:locals		( local variables:
\ "{" starts local list, "|" starts locals defined by "->"


: Rsum { n --- reciprical_sum }
\	= Sum (1/i) for i=1..n

0. n 1+ 1 do
 1. i float f/ f+ loop ;


: Rgsum { start stop --- ranged_reciprical_sum }
\    = Sum (1/i) for i=start..stop

0. stop 1+ start do
 1. i float f/ f+ loop ;


: Acode { n | r --- f }
\ Theoretical information content of a Zipf population
\	= -Sum (p(i)*log-base-2(p(i)) where p(i) = 1/(i*rsum(n)) for i=1..n

n Rsum -> r	( define r
0. n 1+ 1 do
 i float r f* fdup fln fswap f/ f+ loop
2. fln f/ ;


: Ccode { m --- Zipf_info_content }
\ info content of two part encoding of Zipf population distribution,
\ m = # bits in 1st part, population_size = 2^2^m-1
\	= m + Sum (i*Rgsum(2^i,2^(i+1)-1))/Rsum(2^2^m-1) for i=0..2^m-1
\ see: Comparison and Extension of Theories of Zipf and Halstead,
\ by R.E. Prather, The Computer Journal 31:3:248-252, March 1988.

0. 1 m shift 0 do
 1 i shift 1 i 1+ shift 1- Rgsum i float f* f+ loop
1 1 m shift shift 1- Rsum f/ m float f+ ;


: Info ( n -> info_content )
\ Theoretical information content of equal probability objects
\	= Log-base-2(n)
\ Take ceiling to get # bits for token

float fln 2. fln f/ ;


\ ...		Zipf's distribution code density for
\			a population of 2^2^m - 1 objects

\	m    2^2^m-1     2^m      Acode     Ccode    1-Ccode/2^m
\	-    -------    ----      -----     -----    -----------
\	1        3      2.00      1.435     1.455         27%
\	2       15      4.00      3.322     3.365         16%
\	3      255      8.00      6.210     6.254         22%
\	4    65535     16.00     11.140    11.174         30%

\	end of file


