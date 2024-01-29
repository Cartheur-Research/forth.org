'
' *** PR.BAS -- PRINTER Control Chararacter Generator
'
' Default variable type is integer in this module.
DEFINT A-Z

DIM A$(1 TO 15), cmd$(30), Code(30, 3), Labl$(30)
MaxCodes = 30

' Get Defined Commands and their codes
RESTORE
FOR K = 1 TO MaxCodes
     READ cmd$(K)
     IF cmd$(K) = "?" THEN MaxCodes = K - 1: EXIT FOR
     READ Code(K, 1), Code(K, 2), Code(K, 3), Labl$(K)
NEXT K

' Get the command line using the COMMAND$ function.
  Cl$ = COMMAND$
  L = LEN(Cl$)
  IF L = 0 OR Cl$ = " " THEN L = 1: Cl$ = "?"
 
IF Cl$ = "?" THEN
     PRINT
     PRINT "Printer Control Code Generator"
     PRINT "     Usage:  PR xyz"
     PRINT "  Where xyz are translated as follows:"
     FOR K = 1 TO MaxCodes
          PRINT "   "; cmd$(K); " "; Labl$(K)
     NEXT K
    
ELSE FOR I = 1 TO L
        Cr$ = MID$(Cl$, I, 1)
        IF Cr$ > " " THEN
           A$ = CHR$(ASC(Cr$) AND &H5F)
           FOR J = 1 TO MaxCodes
               IF A$ = cmd$(J) THEN
                    FOR K = 1 TO 3
                    IF Code(J, K) > -1 THEN LPRINT CHR$(Code(J, K));
                    NEXT K: EXIT FOR
                    END IF
             NEXT J
          IF J > MaxCodes THEN ERROR 5
        END IF
     NEXT I
END IF
END

 ' Printer Commands & Codes
 DATA G, 27,116,1,  "Graphic Chars"
 DATA I, 27,116,0,  "Italic Chars"
 DATA W, 27,87,1,   "Wide Print "
 DATA N, 27,33,0,   "Normal Print 10/inch"
 DATA E, 27,77,-1,  "Elite Print 12/inch"
 DATA C, 27,15,-1,  "Compressed  "      
 DATA X, 27,33,5,   "Xtra Compressed 20/inch"
 DATA Q, 27,120,1,  "Letter Quality"
 DATA D, 27,120,0,  "Draft Quality"
 DATA B, 27,69,-1,  "Bold (Emphasized)"
 DATA "5",27,108,5, "Indent 5 Chars"
 DATA "0",27,108,0, "Indent 0 Chars"
 DATA F,12,-1,-1,   "Form Feed"
 DATA "?"


