        TTL "68HC12 eForth by Pete Zawasky"
        PLEN 62         ;62 lines per page
        LLEN 132        ;132 characters per line
;
; Created        11/25/96 13:26 ppz
; Last Edit      06/13/97 16:19 ppz
;===============================================================
;
;       68HC12 eForth is a direct threaded eforth meant to be assembled
;       with the Motorola MCUasm M68HC12 Macro Assembler Version 5.3
;       and used on the Motorola M68HC12EVB. The M68HC12EVB used for
;       this example of eForth has external RAM installed in sockets
;       U4 and U6A. The RAM in these sockets is both byte and word
;       addressable. According to the HC12 documentation, there is no
;       penalty (in most cases) for byte accesses not aligned on word
;       boundaries. However, you will notice the liberal use of the
;       directive EVEN and NOPs in the macro expansion.
;
;       Yes, it is true, trying to use a different assembler can provide
;       many hours of 'enjoyment' and perhaps some enlightenment. This
;       particular MASM had difficulty generating an S record file when
;       many ORG directives were used to cause the NAME dictionary to
;       separately grow down towards the CODE dictionary growing up.
;       Let me know if you find an elegant way to do it.
;
;       Most of the words have been tested. There still may be a bug
;       or two in this version of 68HC12 eForth, but it has been working.
;       Add interrupts, fuzzy logic and low speed DSP code and the
;       learning begins.
;
;       by Pete Zawasky
;               PZEF Company
;               518 Hamden RD
;               Annandale, NJ 08801
;               (908) 735-2165
;
;===============================================================
;       References:
;
;       HC11E4TH.V10
;       68hc11 eForth for the Motorola FREEWARE assembler
;       by Karl Lunt
;
;       Subroutine threaded eForth; Version. 2.0, 1991
;       for the Motorola M68HC11EVB Evaluation Board
;       by Richard E. Haskell
;               Dept. of Computer Science and Engineering
;               Oakland University
;               Rochester, Michigan 48309
;
;       eForth 1.0 by Bill Muench and C. H. Ting, 1990
;       Much of the code is derived from the following sources:
;               8086 figForth by Thomas Newman, 1981 and Joe smith, 1983
;               aFORTH by John Rible
;               bFORTH by Bill Muench
;
;       The goal of this implementation is to provide a simple eForth Model
;       which can be ported easily to many 8, 16, 24 and 32 bit CPU's.
;       The following attributes make it suitable for CPU's of the '90:
;
;               small machine dependent kernel and portable high level code
;               source code in the MASM format
;               subroutine threaded code
;               separated code and name dictionaries
;               simple vectored terminal and file interface to host computer
;               aligned with the proposed ANS Forth Standard
;               easy upgrade path to optimize for specific CPU
;               easy mixing of Forth and assembly language
;               all assembly language tools can be used directly
;
;       You are invited to implement this Model on your favorite CPU and
;       contribute it to the eForth Library for public use. You may use
;       a portable implementation to advertise more sophisticated and
;       optimized version for commercial purposes. However, you are
;       expected to implement the Model faithfully. The eForth Working
;       Group reserves the right to reject implementation which deviates
;       significantly from this Model.
;
;       As the ANS Forth Standard is still evolving, this Model will
;       change accordingly. Implementations must state clearly the
;       version number of the Model being tracked.
;
;       Representing the eForth Working Group in the Silicon Valley FIG Chapter.
;       Send contributions to:
;
;               Dr. C. H. Ting
;               156 14th Avenue
;               San Mateo, CA 94402
;               (415) 571-7639
;
;===============================================================
;       M68HC12EVB installation notes -- Pete Zawasky  11/25/96
;
;       This installation of eForth is specific to the Motorola
;       M68HC12A4EVB Single Board Computer and the Motorola MCUasm
;       Assembly Language Development Toolset with M68HC12 Macro
;       assembler, Version 5.3.
;
;
;===============================================================
;
;  RAM allocation:
;
;               8000   +-------------------------------+  EM
;                      |                               |
;                      |                               |
;                      |                               |
;               6FF0   +-------------------------------+  NAMEE
;                      |  name dictionary grows down   |
;                      |            \/                 |
;                      |            \/                 |
;                      +-------------------------------+
;                      +-------------------------------+
;                      |            /\                 |
;                      |            /\                 |
;                      |   code dictionary grows up    |
;               4100   +-------------------------------+  CODEE
;                      |                               |
;                      +-------------------------------+
;                      |       EVB init stuff          |
;    EXT RAM    4000   +-------------------------------+  COLDD
;                      |                               |
;                      |                               |
;               0980   +-------------------------------+  RPP
;                      |   return stack grows down     |
;                      |            \/                 |
;                      |            \/                 |
;                      +-------------------------------+
;                      |            /\                 |
;                      |      text input buffer        |
;               08C0   +-------------------------------+  TIBB/SPP
;                      |    data stack grows down      |
;                      |            \/                 |
;                      |            \/                 |
;                      +-------------------------------+
;                      |            /\                 |
;                      |            /\                 |
;                      |    user area grows up         |
;               0802   +-------------------------------+  UPP
;       SRAM    0800   +-------------------------------+  IP
;
;
;       68HC12 Registers used in eForth model
;       D     = A:B                             = 16bit data
;       X     = General Purpose                 = 16bits
;       Y     = Return Stack Pointer, RP        = 16bits
;       SP    = Data Stack Pointer,   SP        = 16bits
; $0800:$0801 = Interpreter Pointer,  IP        = 16bits
;
;       Stack and all variables and tokens are 16 bits wide.
;
;===============================================================

;; Version control

VER:            EQU     1               ;major release version
EXT:            EQU     0               ;minor extension

;; Hardware

;SCI -- Port 0 registers
SC0BDH:         EQU     $00C0           ;SCI Baud Rate Control Register
SC0BDL:         EQU     $00C1           ;SCI Baud Rate Control Register
SC0CR1:         EQU     $00C2           ;SCI Control Register 1
SC0CR2:         EQU     $00C3           ;SCI Control Register 2
SC0SR1:         EQU     $00C4           ;SCI Status Register 1
SC0SR2:         EQU     $00C5           ;SCI Status Register 2
SC0DRH:         EQU     $00C6           ;SCI Data Register High
SC0DRL:         EQU     $00C7           ;SCI Data Register Low

;; Constants

TRUE:           EQU     -1              ;true flag
FALSE:          EQU     0

;       lexicon bits -- Motorola format
COMPO:          EQU     $40             ;lexicon compile only bit
IMEDD:          EQU     $80             ;lexicon immediate bit
MASKK:          EQU     $1F7F           ;lexicon bit mask

CELLL:          EQU     2               ;size of a cell
BASEE:          EQU     10              ;default radix
VOCSS:          EQU     8               ;depth of vocabulary stack

BKSPP:          EQU     8               ;back space
LF:             EQU     10              ;line feed
CRR:            EQU     13              ;carriage return
ERR:            EQU     27              ;error escape
TIC:            EQU     39              ;tick

;               Change these opcodes for your own microprocessor.
JUMP:           EQU     $A706           ;NOPJMP hh ll opcode

;; Memory allocation    CHANGE FOR DIFFERENT ENVIRONMENTS

RAM:            EQU     $4000           ;external RAM, 16K bytes
SRAM:           EQU     $0800           ;internal user RAM, 512 bytes

;Assemble in RAM
US:             EQU     64*CELLL        ;user area size in cells
RTS:            EQU     96*CELLL        ;return stack/TIB size

EM:             EQU     $8000           ;top of external RAM
NAMEE:          EQU     $6FF0           ;name dictionary
CODEE:          EQU     $4100           ;code dictionary
COLDD:          EQU     $4000           ;cold start vector
RPP:            EQU     $0980-2         ;start of return stack (RP0)
TIBB:           EQU     RPP+2-RTS       ;terminal input buffer (TIB)
SPP:            EQU     TIBB            ;start of data stack (SP0)
UPP:            EQU     SRAM+2           ;start of user area (UP0)
IP:             EQU     SRAM            ;reserve 2 bytes for IP

;; Initialize assembly variables

_LINK:          SET     0               ;force a null link
_NAME:          SET     NAMEE           ;initialize name pointer
_CODE:          SET     CODEE           ;initialize code pointer
_USER:          SET     4*CELLL         ;first user variable offset

;; Define assembly macros
;       These must be changed for different microprocessors and
;       for the assembler used.

;       The code generated by COLON$ and CODE$ creates separate
;       NAME and CODE areas, per conventional eForth.  The two
;       areas should grow inward from opposite ends of the memory
;       space during assembly as in the original 8051 code.

;       Presently, the MCUasm Toolset has problems with many ORGs
;       or perhaps too many macro invocations.  Therefore, NAME and
;       CODE areas are entertwined during assembly in this version
;       of eForth. During runtime, the NAME and CODE areas grow
;       towards each other as in the eForth model.

;       Compile a code definition header.

;CODE$:  MACRO   ;LEX,NAME,ADD_BYTE,LABEL
;        EVEN                            ;align to word boundary
;        DC.W    \4                      ;token pointer
;        DC.W    _LINK                   ;link pointer
;_LINK:  SET     *                       ;link points to a name string
;        DC.B    \1,\2                   ;name string
;        IFNE    \3                      ;add fill byte if NAME length
;          DC.B  $00                     ;      is even
;        ENDC
;\4:                                     ;assembly label
;        ENDM

CODE$:  MACRO   ;LEX,NAME,ADD_BYTE,LABEL
        EVEN
        DC.W    \4
        DC.W    _LINK
_LINK:  SET     *
        DC.B    \1,\2
        IFNE    \3
          DC.B  $00
        ENDC
\4:
        ENDM

;       Compile a colon definition header.

COLON$: MACRO   ;LEX,NAME,ADD_BYTE,LABEL
        CODE$   \1,\2,\3,\4
        NOP
        JMP     DOLST                   ;JMP hh ll
        ENDM

;       Compile a user variable header.

USER$:  MACRO   ;LEX,NAME,ADD_BYTE,LABEL
        CODE$   \1,\2,\3,\4
        NOP
        JMP     DOLST                   ;JMP hh ll
        DC.W    DOUSE                   ;doUSER
        DC.W    _USER                   ; and offset
_USER:  SET     _USER+CELLL             ;update user area offset
        ENDM

;===============================================================

;*****  Initialization Routines  *****

;===============================================================
;*****  Start of main program  *****

;; Main entry points and COLD start data


 ORG    COLDD                           ;beginning of cold boot area

;; eForth starts here

ORIG:
                                        ;serial I/O initialization
                LDY     #RPP            ;RP - initialize return stack
                LDS     #SPP            ;SP - initialize data stack
                MOVW    #COLD1,IP       ;IP - point to first instruction
                JMP     _next

;===============================================================
;;  The Forth inner interpreter
;
;  Entry can occur at one of three locations, depending on the action
;  needed.
;
;  Entry at pushd pushes the word in D onto the data stack, then
;  falls into _next.  Use this entry point when leaving a low-level
;  definition that must also push a result (in D) onto the data
;  stack.
;
;  Entry at _next moves to the next low-level definition in the
;  thread.
;
;  Entry at _next1 assumes X already contains the address
;  of the next word.
;
;  Use _next and _next1 when leaving a low-level definition that does
;  not need to save any results onto the data stack.


pushd:          PSHD                    ;save D on data stack
                                        ;   and fall into _next
_next:          LDX     IP
_next1:         TFR     X,D             ;x = ip
                ADDD    #2
                STD     IP              ;ip = ip + 2
                LDX     0,X             ;x = (ip)
                JMP     0,X             ;go to next word

;===============================================================

; COLD start moves the following to USER variables.
; MUST BE IN SAME ORDER AS USER VARIABLES.



 EVEN                                   ;align to cell boundary

UZERO:          DC.W    0               ;reserved space in user area
                DC.W    0
                DC.W    0
                DC.W    0
                DC.W    SPP             ;SP0
                DC.W    RPP             ;RP0
                DC.W    QRX             ;'?KEY
                DC.W    TXSTO           ;'EMIT
                DC.W    ACCEP           ;'EXPECT
                DC.W    KTAP            ;'TAP
                DC.W    TXSTO           ;'ECHO
                DC.W    DOTOK           ;'PROMPT
                DC.W    BASEE           ;BASE
                DC.W    0               ;tmp
                DC.W    0               ;SPAN
                DC.W    0               ;>IN
                DC.W    0               ;#TIB
                DC.W    TIBB            ;TIB
                DC.W    0               ;CSP
                DC.W    INTER           ;'EVAL
                DC.W    NUMBQ           ;'NUMBER
                DC.W    0               ;HLD
                DC.W    0               ;HANDLER
                DC.W    0               ;CONTEXT pointer
                DCB.W   VOCSS,0         ;vocabulary stack
                DC.W    0               ;CURRENT pointer
                DC.W    0               ;vocabulary link pointer
                DC.W    CTOP            ;CP
                DC.W    NTOP            ;NP
                DC.W    LASTN           ;LAST
                DC.W    VFRTH           ;forth
ULAST:
_ULMUZ: SET ULAST-UZERO

;===============================================================

;       CHANGE THE FOLLOWING MACHINE DEPENDENT WORDS

 ORG    CODEE                          ;beginning of the code dictionary

;===============================================================
;; Device dependent I/O

;   BYE         ( -- )
;               Exit eForth.

                CODE$   3,'BYE',0,BYE
                SWI                      ;back to D-Bug12
;                JMP     [$FE00,PCR]     ;Restart D-Bug12

;   ?RX         ( -- c T | F )
;               Return input character and true, or a false if no input.

                CODE$   3,'?RX',0,QRX
                CLRA                    ;only bytes used here
                LDAB    SC0SR1          ;read SCI status reg to check/clear RDRF bit
                ANDB    #$20            ;check only the RDRF flag bit
                BEQ     QRX1            ;return with false only
                LDAB    SC0DRL          ;get character
                PSHD
                LDD     #TRUE           ;true flag
        QRX1:   JMP     pushd           ;and save on stack

;   TX!         ( c -- )
;               Send character c to the output device.

                CODE$   3,'TX!',0,TXSTO
        SEND_CH:                        ;subroutine to send out one byte to SCI
                LDAB    SC0SR1          ;read SCI status reg to check/clear TDRE bit
                ANDB    #$80            ;check only the TDRE flag bit
                BEQ     SEND_CH         ;if TDR is not empty, go back to check it again
                PULD                    ;D = A:c
                STAB    SC0DRL          ;transmit one ASCII character to the screen
                JMP     _next           ;and go do next word

;   !IO         ( -- )
;               Initialize the serial I/O devices.

                CODE$   3,'!IO',0,STOIO
;
                JMP     _next           ;go do next word

;===============================================================
;; The kernel

;   doLIT       ( -- w )
;               Push an inline literal.

                CODE$   COMPO+5,'doLIT',0,DOLIT
                LDX     IP              ;point to next word
                LDD     0,X             ;get it, d = (ip)
                PSHD                    ;save D on data stack
                INX
                INX                     ;ip = ip + 2
                JMP     _next1          ;go to next word

;   doLIST      ( a -- )
;               Process colon list.

                CODE$   COMPO+6,'doLIST',1,DOLST
                LDD     IP
                STD     0,Y             ;save old IP on return stack
                DEY                     ;rp = rp - 2
                DEY
                LDAB    #4              ;ip = ip + 4
                ABX                     ;  (to jump around NOP JMP DOLST)
                JMP     _next1          ;and go to next level

;   EXIT        ( -- )
;               Exit the current word

                CODE$   4,'EXIT',1,EXIT
                INY                     ;rp = rp + 2
                INY
                LDX     0,Y             ;pull old IP from return stack
                JMP     _next1          ;and go to previous level

;   EXECUTE     ( ca -- )
;               Execute the word at ca.

                CODE$   7,'EXECUTE',0,EXECU
                PULX                    ;get ca from TOS
                JMP     0,X

;   next        ( -- )
;               Run time code for the single index loop.
;               : next ( -- ) \ hilevel model
;                 r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;

                CODE$   COMPO+4,'next',1,DONXT
                LDD     2,Y             ;get counter on return stack
                BEQ     DONXT1          ;branch if loop is done
                SUBD    #1              ;no, bump the counter
                STD     2,Y             ;  and replace on return stack
                JMP     BRAN            ;  and branch back to top
        DONXT1: INY                     ;done, drop counter from stack
                INY
                LDX     IP              ;set addr past branch target
                INX
                INX                     ;ip = ip + 2
                JMP     _next1          ;  and go do next word

;   ?branch     ( f -- )
;               Branch if flag is zero.

                CODE$   COMPO+7,'?branch',0,QBRAN
                PULD                    ;get TOS to D
                CPD     #0              ;is it 0?
                BNE     QBRAN1          ;branch if not
                LDX     IP
                LDX     0,X             ;get new IP
                JMP     _next1          ;  and jump there
        QBRAN1: LDX     IP              ;set addr past branch target
                INX
                INX                     ;ip = ip + 2
                JMP     _next1          ;  and jump there

;   branch      ( -- )
;               Branch to an inline address.

                CODE$   COMPO+6,'branch',1,BRAN
                LDX     IP
                LDX     0,X             ;get new IP
                JMP     _next1          ;  and jump there

;   !           ( w a -- )
;               Pop the data stack to memory.

                CODE$   1,'!',0,STORE
                PULX                    ;get the addr
                PULD                    ;get the word
                STD     0,X             ;  and save to addr
                JMP     _next

;   @           ( a -- w )
;               Push memory location to the data stack.

                CODE$   1,'@',0,AT
                PULX                    ;get the addr
                LDD     0,X             ;get the data there
                JMP     pushd           ;  and save it

;   C!          ( c b -- )
;               Pop the data stack to byte memory.

                CODE$   2,'C!',1,CSTOR
                PULX                    ; X = b
                PULD                    ; D = A:B = c
                STAB    0,X             ;be careful with single byte operations
                JMP     _next           ;go do next word

;   C@          ( b -- c )
;               Push byte memory location to the data stack.

                CODE$   2,'C@',1,CAT
                PULX                    ; X = b
                LDAB    0,X             ;be careful with single byte operations
                CLRA                    ; 0:c = A:B = D
                JMP     pushd           ;save on data stack

;   RP@         ( -- a )
;               Push the current RP to the data stack.

                CODE$   3,'RP@',0,RPAT
                PSHY
                JMP     _next           ;go do next word

;   RP!         ( a -- )
;               Set the return stack pointer.

                CODE$   COMPO+3,'RP!',0,RPSTO
                PULY                    ;new return addr
                JMP     _next           ;go do next word

;   R>          ( -- w )
;               Pop the return stack to the data stack.

                CODE$   COMPO+2,'R>',1,RFROM
                INY
                INY
                LDD     0,Y
                JMP     pushd           ;save it

;   R@          ( -- w )
;               Copy top of return stack to the data stack.

                CODE$   2,'R@',1,RAT
                LDD     2,Y
                JMP     pushd

;   >R          ( w -- )
;               Push the data stack to the return stack.

                CODE$   COMPO+2,'>R',1,TOR
                PULD                    ;get data at TOS
                STD     0,Y             ;save on return stack
                DEY                     ;point to next location
                DEY
                JMP     _next

;   SP@         ( -- a )
;               Push the current data stack pointer.

                CODE$   3,'SP@',0,SPAT
                TFR     SP,D            ;get current stack pointer
                JMP     pushd           ;  and save on data stack


;   SP!         ( a -- )
;               Set the data stack pointer.

                CODE$   3,'SP!',0,SPSTO
                PULD                    ;get new stack pointer
                TFR     D,SP
                JMP     _next


;   DROP        ( w -- )
;               Discard top stack item.

                CODE$   4,'DROP',1,DROP
                PULD                    ;bump stack pointer
                JMP     _next

;   DUP         ( w -- w w )
;               Duplicate the top stack item.

                CODE$   3,'DUP',0,DUPP
                LDD     0,SP            ;get TOS
                JMP     pushd           ;copy it

;   SWAP        ( w1 w2 -- w2 w1 )
;               Exchange top two stack items.

                CODE$   4,'SWAP',1,SWAP
                PULX                    ;get top item
                PULD                    ;get second item
                PSHX                    ;reverse order on data stack
                JMP     pushd

;   OVER        ( w1 w2 -- w1 w2 w1 )
;               Copy second stack item to top.

                CODE$   4,'OVER',1,OVER
                LDD     2,SP            ;get second item
                JMP     pushd           ;  and push onto stack

;   0<          ( n -- t )
;               Return true if n is negative.

                CODE$   2,'0<',1,ZLESS
                PULD                    ;get TOS
                TSTA                    ;check high bit
                BMI     ZLESS1          ;branch if negative
                LDD     #FALSE          ;reset the flag
                JMP     pushd
        ZLESS1: LDD     #TRUE           ;set the flag
                JMP     pushd

;   AND         ( w1 w2 -- w )
;               Bitwise AND.

                CODE$   3,'AND',0,ANDD
                PULD                    ;get TOS
                ANDA    0,SP            ;  and do the AND
                ANDB    1,SP
                STD     0,SP            ;save result on stack
                JMP     _next

;   OR          ( w1 w2 -- w )
;               Bitwise inclusive OR.

                CODE$   2,'OR',1,ORR
                PULD                    ;get TOS
                ORAA    0,SP            ;  and do the OR
                ORAB    1,SP
                STD     0,SP            ;save result on stack
                JMP     _next

;   XOR         ( w1 w2 -- w )
;               Bitwise exclusive OR.

                CODE$   3,'XOR',0,XORR
                PULD                    ;get TOS
                EORA    0,SP            ;  and do the XOR
                EORB    1,SP
                STD     0,SP            ;save result on stack
                JMP     _next

;   UM+         ( u1 u2 -- ud )
;               Add two unsigned single numbers and return a double sum.

                CODE$   3,'UM+',0,UPLUS
                PULD                    ;get TOS
                ADDD    0,SP            ;  and add second item
                STD     0,SP            ;save back to stack
                LDD     #0              ;presume cy = 0
                ROLB                    ;move carry into word
                JMP     pushd           ;  and save on stack

;   IDIV        ( num denom -- rem quot )
;               Integer divide 16 by 16

                CODE$   4,'IDIV',1,IDIV
                PULX                    ;put denominator in X
                PULD                    ;put numerator in D
                IDIV                    ; X=quot,D=rem
                PSHD                    ;put remainder on stack
                PSHX                    ;put quotient on stack
                JMP     _next

;   FDIV        ( num denom -- rem quot )
;               Fractional divide 16 by 16

                CODE$   4,'FDIV',1,FDIV
                PULX                    ;put denominator in X
                PULD                    ;put numerator in D
                FDIV                    ; X=quot,D=rem
                PSHD                    ;put remainder on stack
                PSHX                    ;put quotient on stack
                JMP     _next

;   UM*         ( u1 u2 -- ud )
;               Unsigned multiply. Return double product.

                CODE$   3,'UM*',0,UMSTA
                TFR     Y,X             ;save rp
                PULY                    ;put u2 in Y
                PULD                    ;put u1 in D
                EMUL                    ; (Y) ; (D)
                PSHD                    ;put lo 16 bits on data stack
                PSHY                    ;put hi 16 bits on data stack
                TFR     X,Y             ;restore rp
                JMP     _next

;===============================================================
;; System and user variables

;               ALL THE REST OF THE CODE SHOULD BE INDEPENDENT
;               OF ANY PARTICULAR MICROPROCESSOR!

;   doVAR       ( -- a )
;               Run time routine for VARIABLE and CREATE.

                COLON$  COMPO+5,'doVAR',0,DOVAR
                DC.W    RFROM,EXIT

;   UP          ( -- a )
;               Pointer to the user area.

                COLON$  2,'UP',1,UP
                DC.W    DOVAR,UPP

;   doUSER      ( -- a )
;               Run time routine for user variables.

                COLON$  COMPO+6,'doUSER',1,DOUSE
                DC.W    RFROM,AT,UP,AT,PLUS,EXIT

;   SP0         ( -- a )
;               Pointer to bottom of the data stack.

                USER$   3,'SP0',0,SZERO

;   RP0         ( -- a )
;               Pointer to bottom of the return stack.

                USER$   3,'RP0',0,RZERO

;   '?KEY       ( -- a )
;               Execution vector of ?KEY.

                USER$   5,"'?KEY",0,TQKEY

;   'EMIT       ( -- a )
;               Execution vector of EMIT.

                USER$   5,"'EMIT",0,TEMIT

;   'EXPECT     ( -- a )
;               Execution vector of EXPECT.

                USER$   7,"'EXPECT",0,TEXPE

;   'TAP        ( -- a )
;               Execution vector of TAP.

                USER$   4,"'TAP",1,TTAP

;   'ECHO       ( -- a )
;               Execution vector of ECHO.

                USER$   5,"'ECHO",0,TECHO

;   'PROMPT     ( -- a )
;               Execution vector of PROMPT.

                USER$   7,"'PROMPT",0,TPROM

;   BASE        ( -- a )
;               Storage of the radix base for numeric I/O.

                USER$   4,'BASE',1,BASE

;   tmp         ( -- a )
;               A temporary storage location used in parse and find.

                USER$   COMPO+3,'tmp',0,TEMP

;   SPAN        ( -- a )
;               Hold character count received by EXPECT.

                USER$   4,'SPAN',1,SPAN

;   >IN         ( -- a )
;               Hold the character pointer while parsing input stream.

                USER$   3,'>IN',0,INN

;   #TIB        ( -- a )
;               Hold the current count in and address of the terminal input buffer.

                USER$   4,'#TIB',1,NTIB
_USER:  SET     _USER+CELLL             ;hold the base address of the terminal input buffer

;   CSP         ( -- a )
;               Hold the stack pointer for error checking.

                USER$   3,'CSP',0,CSP

;   'EVAL       ( -- a )
;               Execution vector of EVAL.

                USER$   5,"'EVAL",0,TEVAL

;   'NUMBER     ( -- a )
;               Execution vector of NUMBER?.

                USER$   7,"'NUMBER",0,TNUMB

;   HLD         ( -- a )
;               Hold a pointer in building a numeric output string.

                USER$   3,'HLD',0,HLD

;   HANDLER     ( -- a )
;               Hold the return stack pointer for error handling.

                USER$   7,'HANDLER',0,HANDL

;   CONTEXT     ( -- a )
;               An area to specify vocabulary search order.

                USER$   7,'CONTEXT',0,CNTXT
_USER:  SET     _USER+VOCSS*CELLL       ;vocabulary stack

;   CURRENT     ( -- a )
;               Point to the vocabulary to be extended.

                USER$   7,'CURRENT',0,CRRNT
_USER:  SET     _USER+CELLL             ;vocabulary link pointer

;   CP          ( -- a )
;               Point to the top of the code dictionary.

                USER$   2,'CP',1,CP

;   NP          ( -- a )
;               Point to the bottom of the name dictionary.

                USER$   2,'NP',1,NP

;   LAST        ( -- a )
;               Point to the last name in the name dictionary.

                USER$   4,'LAST',1,LAST

;   FORTH     ( -- a )
;               Point to the last name in the name dictionary.

                USER$   5,'forth',0,VFRTH

;===============================================================
;; Common functions

;   FORTH     ( -- )
;       Make FORTH the context vocabulary.

                COLON$  5,'FORTH',0,FORTH
                DC.W     VFRTH,CNTXT,STORE,EXIT

;   doVOC       ( -- )
;               Run time action of VOCABULARY's.
;
;                COLON$  COMPO+5,'doVOC',0,DOVOC
;                DC.W    RFROM,CNTXT,STORE,EXIT
;
;   FORTH       ( -- )
;               Make FORTH the context vocabulary.
;
;                COLON$  5,'FORTH',0,FORTH
;                DC.W    DOVOC
;                DC.W    0                       ;vocabulary head pointer
;                DC.W    0                       ;vocabulary link pointer

;   ?DUP        ( w -- w w | 0 )
;               Dup TOS if it is not zero.

                COLON$  4,'?DUP',1,QDUP
                DC.W    DUPP
                DC.W    QBRAN,QDUP1
                DC.W    DUPP
        QDUP1:  DC.W    EXIT

;   ROT         ( w1 w2 w3 -- w2 w3 w1 )
;               Rot 3rd item to top.

                COLON$  3,'ROT',0,ROT
                DC.W    TOR,SWAP,RFROM,SWAP,EXIT

;   2DROP       ( w w -- )
;               Discard two items on stack.

                COLON$  5,'2DROP',0,DDROP
                DC.W    DROP,DROP,EXIT

;   2DUP        ( w1 w2 -- w1 w2 w1 w2 )
;               Duplicate top two items.

                COLON$  4,'2DUP',1,DDUP
                DC.W    OVER,OVER,EXIT

;   +           ( w w -- sum )
;               Add top two items.

                COLON$  1,'+',0,PLUS
                DC.W    UPLUS,DROP,EXIT

;   NOT         ( w -- w )
;               One's complement of tos.

                COLON$  3,'NOT',0,INVER
                DC.W    DOLIT,-1,XORR,EXIT

;   NEGATE      ( n -- -n )
;               Two's complement of tos.

                COLON$  6,'NEGATE',1,NEGAT
                DC.W    INVER,DOLIT,1,PLUS,EXIT

;   DNEGATE     ( d -- -d )
;               Two's complement of top double.

                COLON$  7,'DNEGATE',0,DNEGA
                DC.W    INVER,TOR,INVER
                DC.W    DOLIT,1,UPLUS
                DC.W    RFROM,PLUS,EXIT

;   -           ( n1 n2 -- n1-n2 )
;               Subtraction.

                COLON$  1,'-',0,SUBB
                DC.W    NEGAT,PLUS,EXIT

;   ABS         ( n -- n )
;               Return the absolute value of n.

                COLON$  3,'ABS',0,ABSS
                DC.W    DUPP,ZLESS
                DC.W    QBRAN,ABS1
                DC.W    NEGAT
        ABS1:   DC.W    EXIT

;   =           ( w w -- t )
;               Return true if top two are equal.

                COLON$  1,'=',0,EQUAL
                DC.W    XORR
                DC.W    QBRAN,EQU1
                DC.W    DOLIT,0,EXIT    ;false flag
        EQU1:   DC.W    DOLIT,-1,EXIT   ;true flag

;   U<          ( u u -- t )
;               Unsigned compare of top two items.

                COLON$  2,'U<',1,ULESS
                DC.W    DDUP,XORR,ZLESS
                DC.W    QBRAN,ULES1
                DC.W    SWAP,DROP,ZLESS,EXIT
        ULES1:  DC.W    SUBB,ZLESS,EXIT

;   <           ( n1 n2 -- t )
;               Signed compare of top two items.

                COLON$  1,'<',0,LESS
                DC.W    DDUP,XORR,ZLESS
                DC.W    QBRAN,LESS1
                DC.W    DROP,ZLESS,EXIT
        LESS1:  DC.W    SUBB,ZLESS,EXIT

;   MAX         ( n n -- n )
;               Return the greater of two top stack items.

                COLON$  3,'MAX',0,MAX
                DC.W    DDUP,LESS
                DC.W    QBRAN,MAX1
                DC.W    SWAP
        MAX1:   DC.W    DROP,EXIT

;   MIN         ( n n -- n )
;               Return the smaller of top two stack items.

                COLON$  3,'MIN',0,MIN
                DC.W    DDUP,SWAP,LESS
                DC.W    QBRAN,MIN1
                DC.W    SWAP
        MIN1:   DC.W    DROP,EXIT

;   WITHIN      ( u ul uh -- t )
;               Return true if u is within the range of ul and uh.

                COLON$  6,'WITHIN',1,WITHI
                DC.W    OVER,SUBB,TOR           ; ul <= u < uh
                DC.W    SUBB,RFROM,ULESS,EXIT

;===============================================================
;; Divide

;   UM/MOD      ( udl udh un -- ur uq )
;               Unsigned divide of a double by a single. Return mod and quotient.

                COLON$  6,'UM/MOD',1,UMMOD
                DC.W    DDUP,ULESS
                DC.W    QBRAN,UMM4
                DC.W    NEGAT,DOLIT,15,TOR
        UMM1:   DC.W    TOR,DUPP,UPLUS
                DC.W    TOR,TOR,DUPP,UPLUS
                DC.W    RFROM,PLUS,DUPP
                DC.W    RFROM,RAT,SWAP,TOR
                DC.W    UPLUS,RFROM,ORR
                DC.W    QBRAN,UMM2
                DC.W    TOR,DROP,DOLIT,1,PLUS,RFROM
                DC.W    BRAN,UMM3
        UMM2:   DC.W    DROP
        UMM3:   DC.W    RFROM
                DC.W    DONXT,UMM1
                DC.W    DROP,SWAP,EXIT
        UMM4:   DC.W    DROP,DDROP
                DC.W    DOLIT,-1,DUPP,EXIT

;   M/MOD       ( d n -- r q )
;               Signed floored divide of double by single. Return mod and quotient.

                COLON$  5,'M/MOD',0,MSMOD
                DC.W    DUPP,ZLESS,DUPP,TOR
                DC.W    QBRAN,MMOD1
                DC.W    NEGAT,TOR,DNEGA,RFROM
        MMOD1:  DC.W    TOR,DUPP,ZLESS
                DC.W    QBRAN,MMOD2
                DC.W    RAT,PLUS
        MMOD2:  DC.W    RFROM,UMMOD,RFROM
                DC.W    QBRAN,MMOD3
                DC.W    SWAP,NEGAT,SWAP
        MMOD3:  DC.W    EXIT

;   /MOD        ( n n -- r q )
;               Signed divide. Return mod and quotient.

                COLON$  4,'/MOD',1,SLMOD
                DC.W    OVER,ZLESS,SWAP,MSMOD,EXIT

;   MOD         ( n n -- r )
;               Signed divide. Return mod only.

                COLON$  3,'MOD',0,MODD
                DC.W    SLMOD,DROP,EXIT

;   /           ( n n -- q )
;               Signed divide. Return quotient only.

                COLON$  1,'/',0,SLASH
                DC.W    SLMOD,SWAP,DROP,EXIT

;===============================================================
;; Multiply

;   *           ( n n -- n )
;               Signed multiply. Return single product.

                COLON$  1,'*',0,STAR
                DC.W    UMSTA,DROP,EXIT

;   M*          ( n n -- d )
;               Signed multiply. Return double product.

                COLON$  2,'M*',1,MSTAR
                DC.W    DDUP,XORR,ZLESS,TOR
                DC.W    ABSS,SWAP,ABSS,UMSTA
                DC.W    RFROM
                DC.W    QBRAN,MSTA1
                DC.W    DNEGA
        MSTA1:  DC.W    EXIT

;   */MOD       ( n1 n2 n3 -- r q )
;               Multiply n1 and n2, then divide by n3. Return mod and quotient.

                COLON$  5,'*/MOD',0,SSMOD
                DC.W    TOR,MSTAR,RFROM,MSMOD,EXIT

;   */          ( n1 n2 n3 -- q )
;               Multiply n1 by n2, then divide by n3. Return quotient only.

                COLON$  2,'*/',1,STASL
                DC.W    SSMOD,SWAP,DROP,EXIT

;===============================================================
;; Miscellaneous

;   CELL+       ( a -- a )
;               Add cell size in bytes to address.

                COLON$  5,'CELL+',0,CELLP
                DC.W    DOLIT,CELLL,PLUS,EXIT

;   CELL-       ( a -- a )
;               Subtract cell size in bytes from address.

                COLON$  5,'CELL-',0,CELLM
                DC.W    DOLIT,0-CELLL,PLUS,EXIT

;   CELLS       ( n -- n )
;               Multiply TOS by cell size in bytes.

                COLON$  5,'CELLS',0,CELLS
                DC.W    DOLIT,CELLL,STAR,EXIT

;   ALIGNED     ( b -- a )
;               Align address to the cell boundary.

                COLON$  7,'ALIGNED',0,ALGND
                DC.W    DUPP,DOLIT,0,DOLIT,CELLL
                DC.W    UMMOD,DROP,DUPP
                DC.W    QBRAN,ALGN1
                DC.W    DOLIT,CELLL,SWAP,SUBB
ALGN1:          DC.W    PLUS,EXIT

;   BL          ( -- 32 )
;               Return 32, the blank character.

                COLON$  2,'BL',1,BLANK
                DC.W    DOLIT,$20,EXIT          ; ' '

;   >CHAR       ( c -- c )
;               Filter non-printing characters.

                COLON$  5,'>CHAR',0,TCHAR
                DC.W    DOLIT,$7F,ANDD,DUPP     ;mask msb
                DC.W    DOLIT,127,BLANK,WITHI   ;check for printable
                DC.W    QBRAN,TCHA1
                DC.W    DROP,DOLIT,$5F          ; '_' replace non-printables
        TCHA1:  DC.W    EXIT

;   DEPTH       ( -- n )
;               Return the depth of the data stack.

                COLON$  5,'DEPTH',0,DEPTH
                DC.W    SPAT,SZERO,AT,SWAP,SUBB
                DC.W    DOLIT,CELLL,SLASH,EXIT

;   PICK        ( ... +n -- ... w )
;               Copy the nth stack item to tos.

                COLON$  4,'PICK',1,PICK
                DC.W    DOLIT,1,PLUS,CELLS
                DC.W    SPAT,PLUS,AT,EXIT

;===============================================================
;; Memory access

;   +!          ( n a -- )
;               Add n to the contents at address a.

                COLON$  2,'+!',1,PSTOR
                DC.W    SWAP,OVER,AT,PLUS
                DC.W    SWAP,STORE,EXIT

;   2!          ( d a -- )
;               Store the double integer to address a.

                COLON$  2,'2!',1,DSTOR
                DC.W    SWAP,OVER,STORE
                DC.W    CELLP,STORE,EXIT

;   2@          ( a -- d )
;               Fetch double integer from address a.

                COLON$  2,'2@',1,DAT
                DC.W    DUPP,CELLP,AT
                DC.W    SWAP,AT,EXIT

;   COUNT       ( b -- b +n )
;               Return count byte of a string and add 1 to byte address.

                COLON$  5,'COUNT',0,COUNT
                DC.W    DUPP,DOLIT,1,PLUS
                DC.W    SWAP,CAT,EXIT

;   HERE        ( -- a )
;               Return the top of the code dictionary.

                COLON$  4,'HERE',1,HERE
                DC.W    CP,AT,EXIT

;   PAD         ( -- a )
;               Return the address of the text buffer above the code dictionary.

                COLON$  3,'PAD',0,PAD
                DC.W    HERE,DOLIT,80,PLUS,EXIT

;   TIB         ( -- a )
;               Return the address of the terminal input buffer.

                COLON$  3,'TIB',0,TIB
                DC.W    NTIB,CELLP,AT,EXIT

;   @EXECUTE    ( a -- )
;               Execute vector stored in address a.

                COLON$  8,'@EXECUTE',1,ATEXE
                DC.W    AT,QDUP                 ;?address or zero
                DC.W    QBRAN,EXE1
                DC.W    EXECU                   ;execute if non-zero
        EXE1:   DC.W    EXIT                    ;do nothing if zero

;   CMOVE       ( b1 b2 u -- )
;               Copy u bytes from b1 to b2.

                COLON$  5,'CMOVE',0,CMOVE
                DC.W    TOR
                DC.W    BRAN,CMOV2
        CMOV1:  DC.W    TOR,DUPP,CAT
                DC.W    RAT,CSTOR
                DC.W    DOLIT,1,PLUS
                DC.W    RFROM,DOLIT,1,PLUS
        CMOV2:  DC.W    DONXT,CMOV1
                DC.W    DDROP,EXIT

;   FILL        ( b u c -- )
;               Fill u bytes of character c to area beginning at b.

                COLON$  4,'FILL',1,FILL
                DC.W    SWAP,TOR,SWAP
                DC.W    BRAN,FILL2
        FILL1:  DC.W    DDUP,CSTOR,DOLIT,1,PLUS
        FILL2:  DC.W    DONXT,FILL1
                DC.W    DDROP,EXIT

;   -TRAILING   ( b u -- b u )
;               Adjust the count to eliminate trailing white space.

                COLON$  9,'-TRAILING',0,DTRAI
                DC.W    TOR
                DC.W    BRAN,DTRA2
        DTRA1:  DC.W    BLANK,OVER,RAT,PLUS,CAT,LESS
                DC.W    QBRAN,DTRA2
                DC.W    RFROM,DOLIT,1,PLUS,EXIT         ;adjusted count
        DTRA2:  DC.W    DONXT,DTRA1
                DC.W    DOLIT,0,EXIT                    ;count = 0

;   PACK$       ( b u a -- a )
;               Build a counted string with u characters from b. Null fill.

                COLON$  5,'PACK$',0,PACKS
                DC.W    ALGND,DUPP,TOR          ;strings only on cell boundary
                DC.W    OVER,DUPP,DOLIT,0
                DC.W    DOLIT,CELLL,UMMOD,DROP  ;count mod cell
                DC.W    SUBB,OVER,PLUS
                DC.W    DOLIT,0,SWAP,STORE      ;null fill cell
                DC.W    DDUP,CSTOR,DOLIT,1,PLUS ;save count
                DC.W    SWAP,CMOVE,RFROM,EXIT   ;move string

;===============================================================
;; Numeric output, single precision

;   DIGIT       ( u -- c )
;               Convert digit u to a character.

                COLON$  5,'DIGIT',0,DIGIT
                DC.W    DOLIT,9,OVER,LESS
                DC.W    DOLIT,7,ANDD,PLUS
                DC.W    DOLIT,$30,PLUS,EXIT     ; '0'

;   EXTRACT     ( n base -- n c )
;               Extract the least significant digit from n.

                COLON$  7,'EXTRACT',0,EXTRC
                DC.W    DOLIT,0,SWAP,UMMOD
                DC.W    SWAP,DIGIT,EXIT

;   <#          ( -- )
;               Initiate the numeric output process.

                COLON$  2,'<#',1,BDIGS
                DC.W    PAD,HLD,STORE,EXIT

;   HOLD        ( c -- )
;               Insert a character into the numeric output string.

                COLON$  4,'HOLD',1,HOLD
                DC.W    HLD,AT,DOLIT,1,SUBB
                DC.W    DUPP,HLD,STORE,CSTOR,EXIT

;   #           ( u -- u )
;               Extract one digit from u and append the digit to output string.

                COLON$  1,'#',0,DIG
                DC.W    BASE,AT,EXTRC,HOLD,EXIT

;   #S          ( u -- 0 )
;               Convert u until all digits are added to the output string.

                COLON$  2,'#S',1,DIGS
        DIGS1:  DC.W    DIG,DUPP
                DC.W    QBRAN,DIGS2
                DC.W    BRAN,DIGS1
        DIGS2:  DC.W    EXIT

;   SIGN        ( n -- )
;               Add a minus sign to the numeric output string.

                COLON$  4,'SIGN',1,SIGN
                DC.W    ZLESS
                DC.W    QBRAN,SIGN1
                DC.W    DOLIT,$2D,HOLD          ; '-'
        SIGN1:  DC.W    EXIT

;   #>          ( w -- b u )
;               Prepare the output string to be TYPE'd.

                COLON$  2,'#>',1,EDIGS
                DC.W    DROP,HLD,AT
                DC.W    PAD,OVER,SUBB,EXIT

;   str         ( w -- b u )
;               Convert a signed integer to a numeric string.

                COLON$  3,'str',0,STR
                DC.W    DUPP,TOR,ABSS
                DC.W    BDIGS,DIGS,RFROM
                DC.W    SIGN,EDIGS,EXIT

;   HEX         ( -- )
;               Use radix 16 as base for numeric conversions.

                COLON$  3,'HEX',0,HEX
                DC.W    DOLIT,16,BASE,STORE,EXIT

;   DECIMAL     ( -- )
;               Use radix 10 as base for numeric conversions.

                COLON$  7,'DECIMAL',0,DECIM
                DC.W    DOLIT,10,BASE,STORE,EXIT

;===============================================================
;; Numeric input, single precision

;   DIGIT?      ( c base -- u t )
;               Convert a character to its numeric value. A flag indicates success.

                COLON$  6,'DIGIT?',1,DIGTQ
                DC.W    TOR,DOLIT,$30,SUBB      ; '0'
                DC.W    DOLIT,9,OVER,LESS
                DC.W    QBRAN,DGTQ1
                DC.W    DOLIT,7,SUBB
                DC.W    DUPP,DOLIT,10,LESS,ORR
        DGTQ1:  DC.W    DUPP,RFROM,ULESS,EXIT

;   NUMBER?     ( a -- n T | a F )
;               Convert a number string to integer. Push a flag on TOS.

                COLON$  7,'NUMBER?',0,NUMBQ
                DC.W    BASE,AT,TOR,DOLIT,0,OVER,COUNT
                DC.W    OVER,CAT,DOLIT,$24,EQUAL        ; '$'
                DC.W    QBRAN,NUMQ1
                DC.W    HEX,SWAP,DOLIT,1,PLUS
                DC.W    SWAP,DOLIT,1,SUBB
        NUMQ1:  DC.W    OVER,CAT,DOLIT,$2D,EQUAL,TOR    ; '-'
                DC.W    SWAP,RAT,SUBB,SWAP,RAT,PLUS,QDUP
                DC.W    QBRAN,NUMQ6
                DC.W    DOLIT,1,SUBB,TOR
        NUMQ2:  DC.W    DUPP,TOR,CAT,BASE,AT,DIGTQ
                DC.W    QBRAN,NUMQ4
                DC.W    SWAP,BASE,AT,STAR,PLUS,RFROM
                DC.W    DOLIT,1,PLUS
                DC.W    DONXT,NUMQ2
                DC.W    RAT,SWAP,DROP
                DC.W    QBRAN,NUMQ3
                DC.W    NEGAT
        NUMQ3:  DC.W    SWAP
                DC.W    BRAN,NUMQ5
        NUMQ4:  DC.W    RFROM,RFROM,DDROP,DDROP,DOLIT,0
        NUMQ5:  DC.W    DUPP
        NUMQ6:  DC.W    RFROM,DDROP
                DC.W    RFROM,BASE,STORE,EXIT

;===============================================================
;; Basic I/O

;   ?KEY        ( -- c T | F )
;               Return input character and true, or a false if no input.

                COLON$  4,'?KEY',1,QKEY
                DC.W    TQKEY,ATEXE,EXIT

;   KEY         ( -- c )
;               Wait for and return an input character.

                COLON$  3,'KEY',0,KEY
        KEY1:   DC.W    QKEY
                DC.W    QBRAN,KEY1
                DC.W    EXIT

;   EMIT        ( c -- )
;               Send a character to the output device.

                COLON$  4,'EMIT',1,EMIT
                DC.W    TEMIT,ATEXE,EXIT

;   NUF?        ( -- t )
;               Return false if no input, else pause and if CR return true.

                COLON$  4,'NUF?',1,NUFQ
                DC.W    QKEY,DUPP
                DC.W    QBRAN,NUFQ1
                DC.W    DDROP,KEY,DOLIT,CRR,EQUAL
        NUFQ1:  DC.W    EXIT

;   PACE        ( -- )
;               Send a pace character for the file downloading process.

                COLON$  4,'PACE',1,PACE
                DC.W    DOLIT,11,EMIT,EXIT

;   SPACE       ( -- )
;               Send the blank character to the output device.

                COLON$  5,'SPACE',0,SPACE
                DC.W    BLANK,EMIT,EXIT

;   SPACES      ( +n -- )
;               Send n spaces to the output device.

                COLON$  6,'SPACES',1,SPACS
                DC.W    DOLIT,0,MAX,TOR
                DC.W    BRAN,CHAR2
        CHAR1:  DC.W    SPACE
        CHAR2:  DC.W    DONXT,CHAR1
                DC.W    EXIT

;   TYPE        ( b u -- )
;               Output u characters from b.

                COLON$  4,'TYPE',1,TYPEE
                DC.W    TOR
                DC.W    BRAN,TYPE2
        TYPE1:  DC.W    DUPP,CAT,EMIT
                DC.W    DOLIT,1,PLUS
        TYPE2:  DC.W    DONXT,TYPE1
                DC.W    DROP,EXIT

;   CR          ( -- )
;               Output a carriage return and a line feed.

                COLON$  2,'CR',1,CR
                DC.W    DOLIT,CRR,EMIT
                DC.W    DOLIT,LF,EMIT           ;auto lf
                DC.W    EXIT

;   do$         ( -- a )
;               Return the address of a compiled string.

                COLON$  COMPO+3,'do$',0,DOSTR
                DC.W    RFROM,RAT,RFROM,COUNT,PLUS
                DC.W    ALGND,TOR,SWAP,TOR,EXIT

;   $"|         ( -- a )
;               Run time routine compiled by $". Return address of a compiled string.

                COLON$  COMPO+3,'$"|',0,STRQP
                DC.W    DOSTR,EXIT              ;force a call to do$

;   ."|         ( -- )
;               Run time routine of ." . Output a compiled string.

                COLON$  COMPO+3,'."|',0,DOTQP
                DC.W    DOSTR,COUNT,TYPEE,EXIT

;   .R          ( n +n -- )
;               Display an integer in a field of n columns, right justified.

                COLON$  2,'.R',1,DOTR
                DC.W    TOR,STR,RFROM,OVER,SUBB
                DC.W    SPACS,TYPEE,EXIT

;   U.R         ( u +n -- )
;               Display an unsigned integer in n column, right justified.

                COLON$  3,'U.R',0,UDOTR
                DC.W    TOR,BDIGS,DIGS,EDIGS
                DC.W    RFROM,OVER,SUBB
                DC.W    SPACS,TYPEE,EXIT

;   U.          ( u -- )
;               Display an unsigned integer in free format.

                COLON$  2,'U.',1,UDOT
                DC.W    BDIGS,DIGS,EDIGS
                DC.W    SPACE,TYPEE,EXIT

;   .           ( w -- )
;               Display an integer in free format, preceeded by a space.

                COLON$  1,'.',0,DOT
                DC.W    BASE,AT,DOLIT,10,XORR   ;?decimal
                DC.W    QBRAN,DOT1
                DC.W    UDOT,EXIT               ;no, display unsigned
        DOT1:   DC.W    STR,SPACE,TYPEE,EXIT    ;yes, display signed

;   ?           ( a -- )
;               Display the contents in a memory cell.

                COLON$  1,'?',0,QUEST
                DC.W    AT,DOT,EXIT

;===============================================================
;; Parsing

;   parse       ( b u c -- b u delta ; <string> )
;               Scan string delimited by c. Return found string and its offset.

                COLON$  5,'parse',0,PARS
                DC.W    TEMP,STORE,OVER,TOR,DUPP
                DC.W    QBRAN,PARS8
                DC.W    DOLIT,1,SUBB,TEMP,AT,BLANK,EQUAL
                DC.W    QBRAN,PARS3
                DC.W    TOR
        PARS1:  DC.W    BLANK,OVER,CAT          ;skip leading blanks ONLY
                DC.W    SUBB,ZLESS,INVER
                DC.W    QBRAN,PARS2
                DC.W    DOLIT,1,PLUS
                DC.W    DONXT,PARS1
                DC.W    RFROM,DROP,DOLIT,0,DUPP,EXIT
        PARS2:  DC.W    RFROM
        PARS3:  DC.W    OVER,SWAP
                DC.W    TOR
        PARS4:  DC.W    TEMP,AT,OVER,CAT,SUBB   ;scan for delimiter
                DC.W    TEMP,AT,BLANK,EQUAL
                DC.W    QBRAN,PARS5
                DC.W    ZLESS
        PARS5:  DC.W    QBRAN,PARS6
                DC.W    DOLIT,1,PLUS
                DC.W    DONXT,PARS4
                DC.W    DUPP,TOR
                DC.W    BRAN,PARS7
        PARS6:  DC.W    RFROM,DROP,DUPP
                DC.W    DOLIT,1,PLUS,TOR
        PARS7:  DC.W    OVER,SUBB
                DC.W    RFROM,RFROM,SUBB,EXIT
        PARS8:  DC.W    OVER,RFROM,SUBB,EXIT

;   PARSE       ( c -- b u ; <string> )
;               Scan input stream and return counted string delimited by c.

                COLON$  5,'PARSE',0,PARSE
                DC.W    TOR,TIB,INN,AT,PLUS     ;current input buffer pointer
                DC.W    NTIB,AT,INN,AT,SUBB     ;remaining count
                DC.W    RFROM,PARS,INN,PSTOR,EXIT

;   .(          ( -- )
;               Output following string up to next ) .

                COLON$  IMEDD+2,'.(',1,DOTPR
                DC.W    DOLIT,$29,PARSE,TYPEE,EXIT      ; ')'

;   (           ( -- )
;               Ignore following string up to next ) . A comment.

                COLON$  IMEDD+1,'(',0,PAREN
                DC.W    DOLIT,$29,PARSE,DDROP,EXIT      ; ')'

;   \           ( -- )
;               Ignore following text till the end of line.

                COLON$  IMEDD+1,'\',0,BKSLA
                DC.W    NTIB,AT,INN,STORE,EXIT

;   CHAR        ( -- c )
;               Parse next word and return its first character.

                COLON$  4,'CHAR',1,CHAR
                DC.W    BLANK,PARSE,DROP,CAT,EXIT

;   TOKEN       ( -- a ; <string> )
;               Parse a word from input stream and copy it to name dictionary.

                COLON$  5,'TOKEN',0,TOKEN
                DC.W    BLANK,PARSE,DOLIT,31,MIN
                DC.W    NP,AT,OVER,SUBB,CELLM
                DC.W    PACKS,EXIT

;   WORD        ( c -- a ; <string> )
;               Parse a word from input stream and copy it to code dictionary.

                COLON$  4,'WORD',1,WORDD
                DC.W    PARSE,HERE,PACKS,EXIT

;===============================================================
;; Dictionary search

;   NAME>       ( na -- ca )
;               Return a code address given a name address.

                COLON$  5,'NAME>',0,NAMET
                DC.W    CELLM,CELLM,AT,EXIT

;   SAME?       ( a a u -- a a f \ -0+ )
;               Compare u cells in two strings. Return 0 if identical.

                COLON$  5,'SAME?',0,SAMEQ
                DC.W    TOR
                DC.W    BRAN,SAME2
        SAME1:  DC.W    OVER,RAT,CELLS,PLUS,AT
                DC.W    OVER,RAT,CELLS,PLUS,AT
                DC.W    SUBB,QDUP
                DC.W    QBRAN,SAME2
                DC.W    RFROM,DROP,EXIT         ;strings not equal
        SAME2:  DC.W    DONXT,SAME1
                DC.W    DOLIT,0,EXIT            ;strings equal

;   find        ( a va -- ca na | a F )
;               Search a vocabulary for a string. Return ca and na if succeeded.

                COLON$  4,'find',1,FIND
                DC.W    SWAP,DUPP,CAT
                DC.W    DOLIT,CELLL,SLASH,TEMP,STORE
                DC.W    DUPP,AT,TOR,CELLP,SWAP
        FIND1:  DC.W    AT,DUPP
                DC.W    QBRAN,FIND6
                DC.W    DUPP,AT,DOLIT,MASKK,ANDD,RAT,XORR
                DC.W    QBRAN,FIND2
                DC.W    CELLP,DOLIT,-1          ;true flag
                DC.W    BRAN,FIND3
        FIND2:  DC.W    CELLP,TEMP,AT,SAMEQ
        FIND3:  DC.W    BRAN,FIND4
        FIND6:  DC.W    RFROM,DROP
                DC.W    SWAP,CELLM,SWAP,EXIT
        FIND4:  DC.W    QBRAN,FIND5
                DC.W    CELLM,CELLM
                DC.W    BRAN,FIND1
        FIND5:  DC.W    RFROM,DROP,SWAP,DROP
                DC.W    CELLM
                DC.W    DUPP,NAMET,SWAP,EXIT

;   NAME?       ( a -- ca na | a F )
;               Search all context vocabularies for a string.

                COLON$  5,'NAME?',0,NAMEQ
                DC.W    CNTXT,DUPP,DAT,XORR     ;?context=also
                DC.W    QBRAN,NAMQ1
                DC.W    CELLM                   ;no, start withcontext
        NAMQ1:  DC.W    TOR
        NAMQ2:  DC.W    RFROM,CELLP,DUPP,TOR    ;next in search order
                DC.W    AT,QDUP
                DC.W    QBRAN,NAMQ3
                DC.W    FIND,QDUP               ;search vocabulary
                DC.W    QBRAN,NAMQ2
                DC.W    RFROM,DROP,EXIT         ;found name
        NAMQ3:  DC.W    RFROM,DROP              ;name not found
                DC.W    DOLIT,0,EXIT            ;false flag

;===============================================================
;; Terminal response

;   ^H          ( bot eot cur -- bot eot cur )
;               Backup the cursor by one character.

                COLON$  2,'^H',1,BKSP
                DC.W    TOR,OVER,RFROM,SWAP,OVER,XORR
                DC.W    QBRAN,BACK1
                DC.W    DOLIT,BKSPP
                DC.W    TECHO,ATEXE,DOLIT,1,SUBB
                DC.W    BLANK,TECHO,ATEXE
                DC.W    DOLIT,BKSPP,TECHO,ATEXE
        BACK1:  DC.W    EXIT

;   TAP         ( bot eot cur c -- bot eot cur )
;               Accept and echo the key stroke and bump the cursor.

                COLON$  3,'TAP',0,TAP
                DC.W    DUPP,TECHO,ATEXE
                DC.W    OVER,CSTOR,DOLIT,1,PLUS,EXIT

;   kTAP        ( bot eot cur c -- bot eot cur )
;               Process a key stroke, CR or backspace.

                COLON$  4,'kTAP',1,KTAP
                DC.W    DUPP,DOLIT,CRR,XORR
                DC.W    QBRAN,KTAP2
                DC.W    DOLIT,BKSPP,XORR
                DC.W    QBRAN,KTAP1
                DC.W    BLANK,TAP,EXIT
        KTAP1:  DC.W    BKSP,EXIT
        KTAP2:  DC.W    DROP,SWAP,DROP,DUPP,EXIT

;   accept      ( b u -- b u )
;               Accept characters to input buffer. Return with actual count.

                COLON$  6,'accept',1,ACCEP
                DC.W    OVER,PLUS,OVER
        ACCP1:  DC.W    DDUP,XORR
                DC.W    QBRAN,ACCP4
                DC.W    KEY,DUPP
;                DC.W    BLANK,SUBB,DOLIT,95,ULESS
                DC.W    BLANK,DOLIT,127,WITHI
                DC.W    QBRAN,ACCP2
                DC.W    TAP
                DC.W    BRAN,ACCP3
        ACCP2:  DC.W    TTAP,ATEXE
        ACCP3:  DC.W    BRAN,ACCP1
        ACCP4:  DC.W    DROP,OVER,SUBB,EXIT

;   EXPECT      ( b u -- )
;               Accept input stream and store count in SPAN.

                COLON$  6,'EXPECT',1,EXPEC
                DC.W    TEXPE,ATEXE,SPAN,STORE,DROP,EXIT

;   QUERY       ( -- )
;               Accept input stream to terminal input buffer.

                COLON$  5,'QUERY',0,QUERY
                DC.W    TIB,DOLIT,80,TEXPE,ATEXE,NTIB,STORE
                DC.W    DROP,DOLIT,0,INN,STORE,EXIT

;===============================================================
;; Error handling

;   CATCH       ( ca -- 0 | err# )
;               Execute word at ca and set up an error frame for it.

                COLON$  5,'CATCH',0,CATCH
                DC.W    SPAT,TOR,HANDL,AT,TOR   ;save error frame
                DC.W    RPAT,HANDL,STORE,EXECU  ;execute
                DC.W    RFROM,HANDL,STORE       ;restore error frame
                DC.W    RFROM,DROP,DOLIT,0,EXIT ;no error

;   THROW       ( err# -- err# )
;               Reset system to current local error frame an update error flag.

                COLON$  5,'THROW',0,THROW
                DC.W    HANDL,AT,RPSTO          ;restore return stack
                DC.W    RFROM,HANDL,STORE       ;restore handler frame
                DC.W    RFROM,SWAP,TOR,SPSTO    ;restore data stack
                DC.W    DROP,RFROM,EXIT

;   NULL$       ( -- a )
;               Return address of a null string with zero count.

                COLON$  5,'NULL$',0,NULLS
                DC.W    DOVAR                   ;emulate CREATE
                DC.W    0
                DC.B    99,111,121,111,116,101
 EVEN            ;$ALIGN

;   ABORT       ( -- )
;               Reset data stack and jump to QUIT.

                COLON$  5,'ABORT',0,ABORT
                DC.W    NULLS,THROW

;   abort"      ( f -- )
;               Run time routine of ABORT" . Abort with a message.

                COLON$  COMPO+6,'abort"',1,ABORQ
                DC.W    QBRAN,ABOR1             ;text flag
                DC.W    DOSTR,THROW             ;pass error string
        ABOR1:  DC.W    DOSTR,DROP,EXIT         ;drop error

;===============================================================
;; The text interpreter

;   $INTERPRET  ( a -- )
;               Interpret a word. If failed, try to convert it to an integer.

                COLON$  10,'$INTERPRET',1,INTER
                DC.W    NAMEQ,QDUP              ;?defined
                DC.W    QBRAN,INTE1
;                DC.W    AT,DOLIT,COMPO,ANDD     ;?compile only lexicon bits -- Intel
                DC.W    CAT,DOLIT,COMPO,ANDD    ;?compile only lexicon bits -- Motorola
                DC.W    ABORQ
                DC.B    13,' compile only'
                DC.W    EXECU,EXIT              ;execute defined word
        INTE1:  DC.W    TNUMB,ATEXE             ;convert a number
                DC.W    QBRAN,INTE2
                DC.W    EXIT
        INTE2:  DC.W    THROW                   ;error

;   [           ( -- )
;               Start the text interpreter.

                COLON$  IMEDD+1,'[',0,LBRAC
                DC.W    DOLIT,INTER,TEVAL,STORE,EXIT

;   .OK         ( -- )
;               Display 'ok' only while interpreting.

                COLON$  3,'.OK',0,DOTOK
                DC.W    DOLIT,INTER,TEVAL,AT,EQUAL
                DC.W    QBRAN,DOTO1
                DC.W    DOTQP
                DC.B    3,' ok'
        DOTO1:  DC.W    CR,EXIT

;   ?STACK      ( -- )
;               Abort if the data stack underflows.

                COLON$  6,'?STACK',1,QSTAC
                DC.W    DEPTH,ZLESS             ;check only for underflow
                DC.W    ABORQ
                DC.B    10,' underflow '
                DC.W    EXIT

;   EVAL        ( -- )
;               Interpret the input stream.

                COLON$  4,'EVAL',1,EVAL
        EVAL1:  DC.W    TOKEN,DUPP,CAT          ;?input stream empty
                DC.W    QBRAN,EVAL2
                DC.W    TEVAL,ATEXE,QSTAC       ;evaluate input, check stack
                DC.W    BRAN,EVAL1
        EVAL2:  DC.W    DROP,TPROM,ATEXE,EXIT   ;prompt

;===============================================================
;; Shell

;   PRESET      ( -- )
;               Reset data stack pointer and the terminal input buffer.

                COLON$  6,'PRESET',1,PRESE
                DC.W    SZERO,AT,SPSTO
                DC.W    DOLIT,TIBB,NTIB,CELLP,STORE,EXIT

;   xio         ( a a a -- )
;               Reset the I/O vectors 'EXPECT, 'TAP, 'ECHO and 'PROMPT.

                COLON$  COMPO+3,'xio',0,XIO
                DC.W    DOLIT,ACCEP,TEXPE,DSTOR
                DC.W    TECHO,DSTOR,EXIT

;   FILE        ( -- )
;               Select I/O vectors for file download.

                COLON$  4,'FILE',1,FILE
                DC.W    DOLIT,PACE,DOLIT,DROP
                DC.W    DOLIT,KTAP,XIO,EXIT

;   HAND        ( -- )
;               Select I/O vectors for terminal interface.

                COLON$  4,'HAND',1,HAND
                DC.W    DOLIT,DOTOK,DOLIT,EMIT
                DC.W    DOLIT,KTAP,XIO,EXIT

;   I/O         ( -- a )
;               Array to store default I/O vectors.

                COLON$  3,'I/O',0,ISLO
                DC.W    DOVAR                   ;emulate CREATE
                DC.W    QRX,TXSTO               ;default I/O vectors

;   CONSOLE     ( -- )
;               Initiate terminal interface.

                COLON$  7,'CONSOLE',0,CONSO
                DC.W    ISLO,DAT,TQKEY,DSTOR    ;restore default I/O device
                DC.W    HAND,EXIT               ;keyboard input

;   QUIT        ( -- )
;               Reset return stack pointer and start text interpreter.

                COLON$  4,'QUIT',1,QUIT
                DC.W    RZERO,AT,RPSTO          ;reset return stack pointer
        QUIT1:  DC.W    LBRAC                   ;start interpretation
        QUIT2:  DC.W    QUERY                   ;get input
                DC.W    DOLIT,EVAL,CATCH,QDUP   ;evaluate input
                DC.W    QBRAN,QUIT2             ;continue till error
                DC.W    TPROM,AT,SWAP           ;save input device
                DC.W    CONSO,NULLS,OVER,XORR   ;?display error message
                DC.W    QBRAN,QUIT3
                DC.W    SPACE,COUNT,TYPEE       ;error message
                DC.W    DOTQP
                DC.B    3,' ? '                 ;error prompt
        QUIT3:  DC.W    DOLIT,DOTOK,XORR        ;?file input
                DC.W    QBRAN,QUIT4
                DC.W    DOLIT,ERR,EMIT          ;file error, tell host
        QUIT4:  DC.W    PRESE                   ;some cleanup
                DC.W    BRAN,QUIT1

;===============================================================
;; The compiler

;   '           ( -- ca )
;               Search context vocabularies for the next word in input stream.

                COLON$  1,"'",0,TICK
                DC.W    TOKEN,NAMEQ             ;?defined
                DC.W    QBRAN,TICK1
                DC.W    EXIT                    ;yes, push code address
        TICK1:  DC.W    THROW                   ;no, error

;   ALLOT       ( n -- )
;               Allocate n bytes to the code dictionary.

                COLON$  5,'ALLOT',0,ALLOT
                DC.W    CP,PSTOR,EXIT           ;adjust code pointer

;   ,           ( w -- )
;               Compile an integer into the code dictionary.

                COLON$  1,$2C,0,COMMA
                DC.W    HERE,DUPP,CELLP         ;cell boundary
                DC.W    CP,STORE,STORE,EXIT     ;adjust code pointer and compile

;   C,          ( c -- )
;               Compile a byte (char) into the code dictionary.

;                COLON$  2,'C,',1,CCOMMA
        EVEN
        DC.W    CCOMMA
        DC.W    _LINK
_LINK:  SET     *
        DC.B    2,'C,'
        DC.B  $00
CCOMMA: NOP
        JMP     DOLST

                DC.W    HERE,DUPP,DOLIT,1,PLUS  ;adjust code pointer
                DC.W    CP,STORE,CSTOR          ;C! byte
                DC.W    EXIT

;   [COMPILE]   ( -- ; <string> )
;               Compile the next immediate word into code dictionary.

                COLON$  IMEDD+9,'[COMPILE]',0,BCOMP
                DC.W    TICK,COMMA,EXIT

;   COMPILE     ( -- )
;               Compile the next address in colon list to code dictionary.

                COLON$  COMPO+7,'COMPILE',0,COMPI
                DC.W    RFROM,DUPP,AT,COMMA     ;compile address
                DC.W    CELLP,TOR,EXIT          ;adjust return address

;   LITERAL     ( w -- )
;               Compile tos to code dictionary as an integer literal.

                COLON$  IMEDD+7,'LITERAL',0,LITER
                DC.W    COMPI,DOLIT,COMMA,EXIT

;   $,"         ( -- )
;               Compile a literal string up to next " .

;                COLON$  3,'$,"',0,STRCQ
        EVEN
        DC.W    STRCQ
        DC.W    _LINK
_LINK:  SET     *
        DC.B    3,'$,"'
STRCQ:  NOP
        JMP     DOLST

                DC.W    DOLIT,$22,WORDD         ; '"' move string to code dictionary
                DC.W    COUNT,PLUS,ALGND        ;calculate aligned end of string
                DC.W    CP,STORE,EXIT           ;adjust the code pointer

;   RECURSE     ( -- )
;               Make the current word available for compilation.

                COLON$  IMEDD+7,'RECURSE',0,RECUR
                DC.W    LAST,AT,NAMET,COMMA,EXIT

;===============================================================
;; Structures

;   FOR         ( -- a )
;               Start a FOR-NEXT loop structure in a colon definition.

                COLON$  IMEDD+3,'FOR',0,FOR
                DC.W    COMPI,TOR,HERE,EXIT

;   BEGIN       ( -- a )
;               Start an infinite or indefinite loop structure.

                COLON$  IMEDD+5,'BEGIN',0,BEGIN
                DC.W    HERE,EXIT

;   NEXT        ( a -- )
;               Terminate a FOR-NEXT loop structure.

                COLON$  IMEDD+4,'NEXT',1,NEXT
                DC.W    COMPI,DONXT,COMMA,EXIT

;   UNTIL       ( a -- )
;               Terminate a BEGIN-UNTIL indefinite loop structure.

                COLON$  IMEDD+5,'UNTIL',0,UNTIL
                DC.W    COMPI,QBRAN,COMMA,EXIT

;   AGAIN       ( a -- )
;               Terminate a BEGIN-AGAIN infinite loop structure.

                COLON$  IMEDD+5,'AGAIN',0,AGAIN
                DC.W    COMPI,BRAN,COMMA,EXIT

;   IF          ( -- A )
;               Begin a conditional branch structure.

                COLON$  IMEDD+2,'IF',1,IFF
                DC.W    COMPI,QBRAN,HERE
                DC.W    DOLIT,0,COMMA,EXIT

;   AHEAD       ( -- A )
;               Compile a forward branch instruction.

                COLON$  IMEDD+5,'AHEAD',0,AHEAD
                DC.W    COMPI,BRAN,HERE,DOLIT,0,COMMA,EXIT

;   REPEAT      ( A a -- )
;               Terminate a BEGIN-WHILE-REPEAT indefinite loop.

                COLON$  IMEDD+6,'REPEAT',1,REPEA
                DC.W    AGAIN,HERE,SWAP,STORE,EXIT

;   THEN        ( A -- )
;               Terminate a conditional branch structure.

                COLON$  IMEDD+4,'THEN',1,THENN
                DC.W    HERE,SWAP,STORE,EXIT

;   AFT         ( a -- a A )
;               Jump to THEN in a FOR-AFT-THEN-NEXT loop the first time through.

                COLON$  IMEDD+3,'AFT',0,AFT
                DC.W    DROP,AHEAD,BEGIN,SWAP,EXIT

;   ELSE        ( A -- A )
;               Start the false clause in an IF-ELSE-THEN structure.

                COLON$  IMEDD+4,'ELSE',1,ELSEE
                DC.W    AHEAD,SWAP,THENN,EXIT

;   WHILE       ( a -- A a )
;               Conditional branch out of a BEGIN-WHILE-REPEAT loop.

                COLON$  IMEDD+5,'WHILE',0,WHILE
                DC.W    IFF,SWAP,EXIT

;   ABORT"      ( -- ; <string> )
;               Conditional abort with an error message.

                COLON$  IMEDD+6,'ABORT"',1,ABRTQ
                DC.W    COMPI,ABORQ,STRCQ,EXIT

;   $"          ( -- ; <string> )
;               Compile an inline string literal.

                COLON$  IMEDD+2,'$"',1,STRQ
                DC.W    COMPI,STRQP,STRCQ,EXIT

;   ."          ( -- ; <string> )
;               Compile an inline string literal to be typed out at run time.

                COLON$  IMEDD+2,'."',1,DOTQ
                DC.W    COMPI,DOTQP,STRCQ,EXIT

;===============================================================
;; Name compiler

;   ?UNIQUE     ( a -- a )
;               Display a warning message if the word already exists.

                COLON$  7,'?UNIQUE',0,UNIQU
                DC.W    DUPP,NAMEQ              ;?name exists
                DC.W    QBRAN,UNIQ1             ;redefinitions are OK
                DC.W    DOTQP
                DC.B    7,' reDef '             ;but warn the user
                DC.W    OVER,COUNT,TYPEE        ;just in case it's not planned
        UNIQ1:  DC.W    DROP,EXIT

;   $,n         ( na -- )
;               Build a new dictionary name using the string at na.

;                COLON$  3,'$,n',0,SNAME
        EVEN
        DC.W    SNAME
        DC.W    _LINK
_LINK:  SET     *
        DC.B    3,'$,n'
SNAME:  NOP
        JMP     DOLST

                DC.W    DUPP,CAT                ;?null input
                DC.W    QBRAN,PNAM1
                DC.W    UNIQU                   ;?redefinition
                DC.W    DUPP,LAST,STORE         ;save na for vocabulary link
                DC.W    HERE,ALGND,SWAP         ;align code address
                DC.W    CELLM                   ;link address
                DC.W    CRRNT,AT,AT,OVER,STORE
                DC.W    CELLM,DUPP,NP,STORE     ;adjust name pointer
                DC.W    STORE,EXIT              ;save code pointer
        PNAM1:  DC.W    STRQP
                DC.B    5,' name'               ;null input
                DC.W    THROW

;===============================================================
;; FORTH compiler

;   $COMPILE    ( a -- )
;               Compile next word to code dictionary as a token or literal.

                COLON$  8,'$COMPILE',1,SCOMP
                DC.W    NAMEQ,QDUP              ;?defined
                DC.W    QBRAN,SCOM2
;                DC.W    AT,DOLIT,IMEDD,ANDD     ;?immediate -- Intel
                DC.W    CAT,DOLIT,IMEDD,ANDD    ;?immediate -- Motorola
                DC.W    QBRAN,SCOM1
                DC.W    EXECU,EXIT              ;it's immediate, execute
        SCOM1:  DC.W    COMMA,EXIT              ;its not immediate, compile
        SCOM2:  DC.W    TNUMB,ATEXE             ;try to convert to number
                DC.W    QBRAN,SCOM3
                DC.W    LITER,EXIT              ;compile number as integer
        SCOM3:  DC.W    THROW                   ;error

;   OVERT       ( -- )
;               Link a new word into the current vocabulary.

                COLON$  5,'OVERT',0,OVERT
                DC.W    LAST,AT,CRRNT,AT,STORE,EXIT

;   ;           ( -- )
;               Terminate a colon definition.

                COLON$  IMEDD+COMPO+1,';',0,SEMIS
                DC.W    COMPI,EXIT,LBRAC,OVERT,EXIT

;   ]           ( -- )
;               Start compiling the words in the input stream.

                COLON$  1,']',0,RBRAC
                DC.W    DOLIT,SCOMP,TEVAL,STORE,EXIT

;   call,       ( ca -- )
;               Assemble a call instruction to ca.

;                COLON$  5,'call,',0,CALLC
        EVEN
        DC.W    CALLC
        DC.W    _LINK
_LINK:  SET     *
        DC.B    5,'call,'
CALLC:  NOP
        JMP     DOLST

                DC.W    DOLIT,JUMP,COMMA        ;insert NOPJMP
                DC.W    COMMA,EXIT              ;insert address
;                DC.W    DOLIT,CALLL,COMMA,HERE  ;Direct Threaded Code
;                DC.W    CELLP,SUBB,COMMA,EXIT   ;DTC 8086 relative call

;   :           ( -- ; <string> )
;               Start a new colon definition using next word as its name.

                COLON$  1,':',0,COLON
                DC.W    TOKEN,SNAME,DOLIT,DOLST
                DC.W    CALLC,RBRAC,EXIT

;   IMMEDIATE   ( -- )
;               Make the last compiled word an immediate word.

                COLON$  9,'IMMEDIATE',0,IMMED
                DC.W    DOLIT,IMEDD,LAST,AT,CAT,ORR     ;Motorola
                DC.W    LAST,AT,CSTOR,EXIT
;                DC.W    DOLIT,IMEDD,LAST,AT,AT,ORR     ;Intel
;                DC.W    LAST,AT,STORE,EXIT

;===============================================================
;; Defining words

;   USER        ( u -- ; <string> )
;               Compile a new user variable.

                COLON$  4,'USER',1,USER
                DC.W    TOKEN,SNAME,OVERT
                DC.W    DOLIT,DOLST,CALLC
                DC.W    COMPI,DOUSE,COMMA,EXIT

;   CREATE      ( -- ; <string> )
;               Compile a new array entry without allocating code space.

                COLON$  6,'CREATE',1,CREAT
                DC.W    TOKEN,SNAME,OVERT
                DC.W    DOLIT,DOLST,CALLC
                DC.W    COMPI,DOVAR,EXIT

;   VARIABLE    ( -- ; <string> )
;               Compile a new variable initialized to 0.

                COLON$  8,'VARIABLE',1,VARIA
                DC.W   CREAT,DOLIT,0,COMMA,EXIT

;===============================================================
;; Tools

;   _TYPE       ( b u -- )
;               Display a string. Filter non-printing characters.

                COLON$  5,'_TYPE',0,UTYPE
                DC.W    TOR                     ;start count down loop
                DC.W    BRAN,UTYP2              ;skip first pass
        UTYP1:  DC.W    DUPP,CAT,TCHAR,EMIT     ;display only printable
                DC.W    DOLIT,1,PLUS            ;increment address
        UTYP2:  DC.W    DONXT,UTYP1             ;loop till done
                DC.W    DROP,EXIT

;   dm+         ( a u -- a )
;               Dump u bytes from a, leaving a+u on the stack.

                COLON$  3,'dm+',0,DMP
                DC.W    OVER,DOLIT,4,UDOTR      ;display address
                DC.W    SPACE,TOR               ;start count down loop
                DC.W    BRAN,PDUM2              ;skip first pass
        PDUM1:  DC.W    DUPP,CAT,DOLIT,3,UDOTR  ;display numeric data
                DC.W    DOLIT,1,PLUS            ;increment address
        PDUM2:  DC.W    DONXT,PDUM1             ;loop till done
                DC.W    EXIT

;   DUMP        ( a u -- )
;               Dump u bytes from a, in a formatted manner.

                COLON$  4,'DUMP',1,DUMP
                DC.W    BASE,AT,TOR,HEX         ;save radix, set hex
                DC.W    DOLIT,16,SLASH          ;change count to lines
                DC.W    TOR                     ;start count down loop
        DUMP1:  DC.W    CR,DOLIT,16,DDUP,DMP    ;display numeric
                DC.W    ROT,ROT
                DC.W    SPACE,SPACE,UTYPE       ;display printable characters
                DC.W    NUFQ,INVER              ;user control
                DC.W    QBRAN,DUMP2
                DC.W    DONXT,DUMP1             ;loop till done
                DC.W    BRAN,DUMP3
        DUMP2:  DC.W    RFROM,DROP              ;cleanup loop stack, early exit
        DUMP3:  DC.W    DROP,RFROM,BASE,STORE   ;restore radix
                DC.W    EXIT

;   .S          ( ... -- ... )
;               Display the contents of the data stack.

                COLON$  2,'.S',1,DOTS
                DC.W    CR,DEPTH                ;stack depth
                DC.W    TOR                     ;start count down loop
                DC.W    BRAN,DOTS2              ;skip first pass
        DOTS1:  DC.W    RAT                     ;index stack
                DC.W    PICK,DOT                ;display contents
        DOTS2:  DC.W    DONXT,DOTS1             ;loop till done
                DC.W    DOTQP
                DC.B    4,' <sp '
                DC.W    EXIT

;   !CSP        ( -- )
;               Save stack pointer in CSP for error checking.

                COLON$  4,'!CSP',1,STCSP
                DC.W    SPAT,CSP,STORE,EXIT     ;save pointer

;   ?CSP        ( -- )
;               Abort if stack pointer differs from that saved in CSP.

                COLON$  4,'?CSP',1,QCSP
                DC.W    SPAT,CSP,AT,XORR        ;compare pointers
                DC.W    ABORQ                   ;abort if different
                DC.B    7,' stacks'
                DC.W    EXIT

;   >NAME       ( ca -- na | F )
;               Convert code address to a name address.

                COLON$  5,'>NAME',0,TNAME
                DC.W    CRRNT                   ;vocabulary link
        TNAM1:  DC.W    CELLP,AT,QDUP           ;check all vocabularies
                DC.W    QBRAN,TNAM4
                DC.W    DDUP
        TNAM2:  DC.W    AT,DUPP                 ;?last word in a vocabulary
                DC.W    QBRAN,TNAM3
                DC.W    DDUP,NAMET,XORR         ;compare
                DC.W    QBRAN,TNAM3
                DC.W    CELLM                   ;continue with next word
                DC.W    BRAN,TNAM2
        TNAM3:  DC.W    SWAP,DROP,QDUP
                DC.W    QBRAN,TNAM1
                DC.W    SWAP,DROP,SWAP,DROP,EXIT
        TNAM4:  DC.W    DROP,DOLIT,0,EXIT

;   .ID         ( na -- )
;               Display the name at address.

                COLON$  3,'.ID',0,DOTID
                DC.W    QDUP                    ;if zero no name
                DC.W    QBRAN,DOTI1
                DC.W    COUNT,DOLIT,$1F,ANDD    ;mask lexicon bits
                DC.W    UTYPE,EXIT              ;display name string
        DOTI1:  DC.W    DOTQP
                DC.B    9,' {noName}'
                DC.W    EXIT

;   SEE         ( -- ; <string> )
;               A simple decompiler.

                COLON$  3,'SEE',0,SEE
                DC.W    TICK                    ;starting address
                DC.W    CR,CELLP
        SEE1:   DC.W    CELLP,DUPP,AT,DUPP      ;?does it contain a zero
                DC.W    QBRAN,SEE2
                DC.W    TNAME                   ;?is it a name
        SEE2:   DC.W    QDUP                    ;name address or zero
                DC.W    QBRAN,SEE3
                DC.W    SPACE,DOTID             ;display name
                DC.W    BRAN,SEE4
        SEE3:   DC.W    DUPP,AT,UDOT            ;display number
        SEE4:   DC.W    NUFQ                    ;user control
                DC.W    QBRAN,SEE1
                DC.W    DROP,EXIT

;   WORDS       ( -- )
;               Display the names in the context vocabulary.

                COLON$  5,'WORDS',0,WORDS
                DC.W    CR,CNTXT,AT             ;only in context
        WORS1:  DC.W    AT,QDUP                 ;?at end of list
                DC.W    QBRAN,WORS2
                DC.W    DUPP,SPACE,DOTID        ;display a name
                DC.W    CELLM,NUFQ              ;user control
                DC.W    QBRAN,WORS1
                DC.W    DROP
        WORS2:  DC.W    EXIT

;===============================================================
;; Hardware reset

;   VER         ( -- n )
;               Return the version number of this implementation.

                COLON$  3,'VER',0,VERSN
                DC.W    DOLIT,VER*256+EXT,EXIT

;   hi          ( -- )
;               Display the sign-on message of eForth.

                COLON$  2,'hi',1,HI
                DC.W    STOIO,CR                ;initialize I/O
                DC.W    DOTQP
                DC.B    15,'68hc12 eForth v'    ;model
                DC.W    BASE,AT,HEX             ;save radix
                DC.W    VERSN,BDIGS,DIG,DIG
                DC.W    DOLIT,$2E,HOLD          ; '.'
                DC.W    DIGS,EDIGS,TYPEE        ;format version number
;                DC.B    VER+'0','.',EXT+'0'      ;version
                DC.W    BASE,STORE,CR,EXIT      ;restore radix

;   'BOOT       ( -- a )
;               The application startup vector.

                COLON$  5,"'BOOT",0,TBOOT
                DC.W    DOVAR
                DC.W    HI                      ;application to boot

;   COLD        ( -- )
;               The hilevel cold start sequence.

                COLON$  4,'COLD',1,COLD
        COLD1:  DC.W    DOLIT,UZERO,DOLIT,UPP
                DC.W    DOLIT,_ULMUZ            ;ULAST-UZERO
                DC.W    CMOVE                   ;initialize user area
                DC.W    PRESE                   ;initialize data stack and TIB
                DC.W    TBOOT,ATEXE             ;application boot
                DC.W    FORTH,CNTXT,AT,DUPP     ;initialize search order
                DC.W    CRRNT,DSTOR,OVERT
                DC.W    QUIT                    ;start interpretation
                DC.W    BRAN
                DC.W    COLD1                   ;just in case

;===============================================================

;   TASK        ( -- )
;               Move TASK to RAM when eForth is in ROM.

 EVEN
_CODE:  SET     *                       ;save code pointer
_LEN:   SET     (4&$1F)/CELLL           ;string cell count, round down
_NAME:  SET     _NAME-((_LEN+3)*CELLL)  ;new header on cell boundary
 ORG    _NAME                           ;set name pointer
        DC.W    _CODE                   ;token pointer
        DC.W    _LINK                   ;link pointer
_LINK:  SET     *                       ;link points to a name string
        DC.B    4,'TASK'                ;name string
        DC.B    $00                     ;add a fill byte
 ORG    _CODE                           ;restore code pointer
        NOP
        JMP     DOLST
                DC.W    EXIT            ;do nothing but provide link

;===============================================================

LASTN:          EQU     _NAME+4                 ;last name address in name dictionary

NTOP:           EQU     _NAME-0                 ;next available memory in name dictionary
CTOP:           EQU     *+0                     ;next available memory in code dictionary

 END

;===============================================================










 