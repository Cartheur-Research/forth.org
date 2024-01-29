
   \ FCONTROL.4TH                                Ken Merk   June/96
   \ WINFORTH


   \ ******************** Parallel Port Interface *********************


    ASM                               \ Loads the Forth assembler

    DECIMAL

        64 8 @L                       \ Look for active LPT1 port
               0= .IF                 \ If no port found then abort
               
               CLS
               23 8  GOTOXY .( Parallel printer port not found.)
               CLOSE QUIT

                  .THEN 

       64 8 @L EQU #PORT                \ Find port addr for printer card
                                        \ assign to constant #PORT

   

    1   CONSTANT  FAN            \ assign each Device its binary weighting
    2   CONSTANT  DRILL 
    4   CONSTANT  PUMP
    8   CONSTANT  SPRINKLER 
    16  CONSTANT  HEATER
    32  CONSTANT  LIGHT
    64  CONSTANT  MOTOR 
    128 CONSTANT  VALVE



    CODE BSET  ( b  #port -- )    \ will SET each bit in #port that matches 
    CX POP                        \ every high bit in byte B.
    DX, TOS MOV                       
    AX, DX IN
    AL, CL OR
    DX, AL OUT
    TOS POP
    NEXT,
    END-CODE


    CODE BRESET  ( b  #port -- )  \ will RESET each bit in #port that matches
    CX POP                        \ every high bit in byte b.
    CX NOT
    DX, TOS MOV 
    AX, DX IN
    AL, CL AND
    DX, AL OUT
    TOS POP
    NEXT,
    END-CODE



   : >ON      ( b  -- )      #PORT BSET    ;     \ turn ON device
   : >OFF     ( b  -- )      #PORT BRESET  ;     \ turn OFF device
   

   : KILL     ( -- )         00 #PORT pc!  ;     \ turn OFF all devices  
   : ALL-ON   ( -- )         255 #PORT pc! ;     \ turn ON all devices
   : WRITE.PORT  ( b -- )    #PORT pc!   ;       \ WRITE byte to port
   
    KILL                                         \ kill all LEDs


   \ *********************** Control Dialog box *****************************



   200 CONSTANT ID_FANON          \ Assign each control an ID number
   201 CONSTANT ID_FANOFF         \ which corresponds to a "ID_" constant
   202 CONSTANT ID_DRILLON        \ to make code easier to follow.
   203 CONSTANT ID_DRILLOFF
   204 CONSTANT ID_PUMPON
   205 CONSTANT ID_PUMPOFF
   206 CONSTANT ID_SPRINKON
   207 CONSTANT ID_SPRINKOFF
   208 CONSTANT ID_HEATERON
   209 CONSTANT ID_HEATEROFF
   210 CONSTANT ID_LIGHTON
   211 CONSTANT ID_LIGHTOFF
   212 CONSTANT ID_MOTORON
   213 CONSTANT ID_MOTOROFF
   214 CONSTANT ID_VALVEON
   215 CONSTANT ID_VALVEOFF
   216 CONSTANT ID_ALLON
   217 CONSTANT ID_KILL
   218 CONSTANT ID_FUNC1
   219 CONSTANT ID_FUNC2
   220 CONSTANT ID_FUNC3
   221 CONSTANT ID_FUNC4

   
    " Machine Controller"                                \ Caption text       
    35 10  235 175  WS_CAPTION  WS_POPUP D+              \ Size and style
                    WS_SYSMENU D+  DS_MODALFRAME D+      \ of dialog box     
                    DIALOG  CONTROLDLG                   \ Dialog name


   \ *************************** Button Array1 ****************************  



    "     "      12  10    112 143   -1                   \ Border around
                 WS_BORDER WS_VISIBLE D+ WS_CHILD D+      \ button array1
                 SS_BLACKFRAME D+  " STATIC" CONTROL


   \ Create button array1- Button text, x y position in box, width and height
   \ of button, ID that identifies which button.

    " Fan On"          20   19    45 14  ID_FANON      PUSHBUTTON
    " Fan Off"         69   19    45 14  ID_FANOFF     PUSHBUTTON
    " Drill On"        20   35    45 14  ID_DRILLON    PUSHBUTTON
    " Drill Off"       69   35    45 14  ID_DRILLOFF   PUSHBUTTON
    " Pump On"         20   51    45 14  ID_PUMPON     PUSHBUTTON
    " Pump Off"        69   51    45 14  ID_PUMPOFF    PUSHBUTTON
    " Sprinkler On"    20   67    45 14  ID_SPRINKON   PUSHBUTTON 
    " Sprinkler Off"   69   67    45 14  ID_SPRINKOFF  PUSHBUTTON 
    " Heater On"       20   83    45 14  ID_HEATERON   PUSHBUTTON
    " Heater Off"      69   83    45 14  ID_HEATEROFF  PUSHBUTTON
    " Light On"        20   99    45 14  ID_LIGHTON    PUSHBUTTON 
    " Light Off"       69   99    45 14  ID_LIGHTOFF   PUSHBUTTON
    " Motor On"        20   115   45 14  ID_MOTORON    PUSHBUTTON
    " Motor Off"       69   115   45 14  ID_MOTOROFF   PUSHBUTTON  
    " Valve On"        20   131   45 14  ID_VALVEON    PUSHBUTTON 
    " Valve Off"       69   131   45 14  ID_VALVEOFF   PUSHBUTTON
    "  Device Control" 20   6     51 10       -1       LTEXT


   \ *************************** Button Array2 *****************************


    "     "      135 10     88 143  -1                 \ Border around
                 WS_BORDER WS_VISIBLE D+ WS_CHILD D+   \ button array2
                 SS_BLACKFRAME D+  " STATIC" CONTROL


   \ Create button array2- Button text, x y position in box, width and height
   \ of button, ID that identifies which button.

    " Preset Function #1" 145 19    68 18  ID_FUNC1   PUSHBUTTON
    " Preset Function #2" 145 39    68 18  ID_FUNC2   PUSHBUTTON
    " Preset Function #3" 145 59    68 18  ID_FUNC3   PUSHBUTTON
    " Preset Function #4" 145 79    68 18  ID_FUNC4   PUSHBUTTON
    " ALL ON"             145 99    68 18  ID_ALLON   PUSHBUTTON 
    " KILL"               145 119   68 26  ID_KILL    PUSHBUTTON
    " Quit"               173 158   40 14  IDCANCEL   PUSHBUTTON
    "  Group Control"     145 6     48 10   -1        LTEXT

    END-DIALOG


   \ Case statement takes button ID's given by the message handler
   \ to determine what action to take.
  
    : DO.BUTTON
               CASE
                ID_FANON      OF    FAN       >ON   ENDOF
                ID_FANOFF     OF    FAN       >OFF  ENDOF
                ID_DRILLON    OF    DRILL     >ON   ENDOF
                ID_DRILLOFF   OF    DRILL     >OFF  ENDOF
                ID_PUMPON     OF    PUMP      >ON   ENDOF
                ID_PUMPOFF    OF    PUMP      >OFF  ENDOF
                ID_SPRINKON   OF    SPRINKLER >ON   ENDOF
                ID_SPRINKOFF  OF    SPRINKLER >OFF  ENDOF
                ID_HEATERON   OF    HEATER    >ON   ENDOF
                ID_HEATEROFF  OF    HEATER    >OFF  ENDOF
                ID_LIGHTON    OF    LIGHT     >ON   ENDOF
                ID_LIGHTOFF   OF    LIGHT     >OFF  ENDOF
                ID_MOTORON    OF    MOTOR     >ON   ENDOF
                ID_MOTOROFF   OF    MOTOR     >OFF  ENDOF
                ID_VALVEON    OF    VALVE     >ON   ENDOF
                ID_VALVEOFF   OF    VALVE     >OFF  ENDOF 
                ID_ALLON      OF    ALL-ON          ENDOF    
                ID_KILL       OF    KILL            ENDOF
                IDCANCEL      OF    0 CLOSEDLG      ENDOF
                ID_FUNC1      OF    5  WRITE.PORT   ENDOF
                ID_FUNC2      OF    9  WRITE.PORT   ENDOF
                ID_FUNC3      OF    10 WRITE.PORT   ENDOF
                ID_FUNC4      OF    6  WRITE.PORT   ENDOF 
              ENDCASE ; 



   \ Dialog box message handler intrercepts WM_INITDIALOG and WM_COMMAND
   \ messages and then processes them.
   \ Button ID's are taken from WM_COMMAND's wParam and sent to DO.BUTTON
   \ case statement which determines what action to take. 


   : CONTROLDLGPROC                       
                  wMsg
                  CASE
                 WM_INITDIALOG OF TRUE ENDOF   
                 WM_COMMAND   OF wParam  DO.BUTTON  TRUE ENDOF
                 FALSE SWAP   
                 ENDCASE  ;



   \ Runs the dialog template with the associated dialog message handler
   \ which starts the program. 



  : MAIN        CONTROLDLG ['] CONTROLDLGPROC  RUNDLG DROP   ;

