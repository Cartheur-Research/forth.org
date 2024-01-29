\ Test for ANS FORmula TRANslator

marker -test
fvariable a
fvariable b
fvariable c
fvariable x
fvariable w

: test0   f" b+c"  cr  fe.
          f" b-c"  cr  fe.
          f" (b-c)/(b+c)"  cr fe.  ;

3.e0 b f!
4.e0 c f!
see test0
test0

: test1   f" a=b*c-3.17e-5/tanh(w)+abs(x)"  a f@  cr fe.  ;
1.e-3 w f!
-2.5e0 x f!
cr cr
see test1
test1

0 [IF]
Output should look like:

: test0
  c f@ b f@ f+ cr fe. c f@ fnegate b f@ f+ cr fe. c f@ fnegate b f@
  f+ c f@ b f@ f+ f/ cr fe. ;
7.00000000000000E0
-1.00000000000000E0
-142.857142857143E-3


: test1
  x f@ fabs 3.17000000000000E-5 w f@ ftanh f/ fnegate b f@ c f@ f* f+
  f+ a f! a f@ cr fe. ;
14.4682999894333E0  ok

with more or fewer places.

[THEN]


