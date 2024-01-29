// FVARIABLE || sumx sumxz sumz sumx2 \\

: Lsq-Init              ( -- )
    0 n !
    // let | = 0: | sumx sumxz sumx2 sumz \\
;

: Calc-Det              ( F: -- d )
    let {n @ S>F}*sumx2 - sumx*{FDUP}:
;

: Estimate              ( F: -- b a )
    let x = {Calc-Det}:

    \ Calculate b and a
    let (sumx2*sumz - sumx*sumxz) / x, ({n @ S>F}*sumxz - sumx*sumz) / x:

;

: Lsq                   ( --<infile>-- )
    Lsq-Init

    next_file                  ( str len)
    
    R/O OPEN-FILE ABORT" Unable to open input data file. "
        TO fin                 ( )
    
    CR
    
    fin get-int  DUP n !       ( n)
    
    0 DO                       ( )
            I .
        let x = {fin get-float}:  \ Get X point
            x F.
        let sumx = sumx + x:
        let sumx2 = sumx2 + x*{FDUP}:
        
        let z = {fin get-float}:  \ Get Z point
            z F.
        let sumz = sumz + z:
        let sumxz = sumxz + x*z:
            CR
    LOOP
        
    fin CLOSE-FILE DROP
    
    Estimate               ( b a)
    ." slope (a) : " F.
    ." intercept (b) : " F. CR  ( )
;

