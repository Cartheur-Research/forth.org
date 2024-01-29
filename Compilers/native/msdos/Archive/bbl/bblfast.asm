
	SUBTTL	FAST	Why BBL is so Fast
	SUBTTL	FAST	Why BBL is so Fast
	%OUT	FAST	Why BBL is so Fast
	PAGE	+
;==================================
COMMENT |
 
DESIGN OF BBL FORTH INTERNALS - WHY IT IS SO FAST
=================================================
 
Some people have wondered how BBL a public domain 32 bit Forth
compiler could run so quickly.	It is a true 32 bit compiler
with 32 bit stack items.  All addresses are 32 bits so you can
address the full megabyte.  You can fill that meg with data or
high level code without special bank switching code.  It is not
a 16-bit Forth masquerading as a 32 bit version.
 
Here are the main reasons for its speed:
 
Top of Stack
============
 
BBL Forth keeps the top of stack in a register pair CX:BX rather
than in ram as is traditional.	This saves a tremendous amount
of pushing and popping to the slow RAM-based stack.  For example
DUP is implemented with two PUSHes rather than two POPs and four
PUSHes as in traditional designs.
 
Absolute Addresses
==================
 
The programmer works directly with absolute segment:offset
addresses.  Words like C@ expect an ABSOLUTE address.  You only
resort to using flat linear relative addresses when you work
with arrays larger than 64K.  This saves a lot of translation
from relative to absolute segment:offset addresses that must
occur in words like @ ! CMOVE in traditional designs.  The
translation is quite a production.  In traditional designs the
following sort of thing would need to happen inside every @ and !.
 
;	( MASM code to implement REL> )
;	( 32-bit relative address -- absolute SEG:OFF )
;	Also handles negative relative addresses that point prior to
;	CS:.
;	( CX:BX -- CX seg BX off)
	 MOV	 DX,CX
	 MOV	 CX,BX
	 AND	 BX,0Fh 	; ensure offset lies in [0..15]
				; BX now has the final offset
	 SHR	 DX,1		; shift DX:CX right 4 bits
	 RCR	 CX,1		; multi-register shifts must be done
	 SHR	 DX,1		; a bit at a time even on NEC chips
	 RCR	 CX,1		; This code is even SLOWER than it
	 SHR	 DX,1		; looks.  When you have that many
	 RCR	 CX,1		; two cycle instructions in a row
	 SHR	 DX,1		; the 8086 prefetch instruction
	 RCR	 CX,1		; buffer gets exhausted, and the
	 MOV	 AX,CS		; chip slows down while it waits
				; for the next instruction byte.
	 ADD	 CX,AX		; rel seg:off to abs seg:off
	 QNEXT
 
Direct Threaded Inner Interpreter
=================================
 
The main secret of BBL's speed is its inner interpreter.  BBL is
a direct threaded interpreter rather than an indirect one as is
traditional.  The inner interpreter is very fast:
 
NEXT:
	LODSW
	JMP	AX
 
This inner interpreter takes 3 bytes and 23 cycles.  Because the
inner interpreter is so short we can expand it inline to save
the JMP NEXT for a further saving of 15 cycles.  The inner
interpreter gets executed once per word, so it HAS to be fast.
The inner interpreter (sometimes called NEXT) is the glue used
at the end of every single code word to invoke the next word.
It is the equivalent of the call/return process in standard
assembler.
 
This inner interpreter was chosen over about ten other
possibilities.	This one was the fastest overall even though it
makes dictionary structure a bit wild and makes the words >BODY
BODY> >NAME etc. almost impossible.  It also limits you to
writing 64K worth of your code in assembler, the rest has to be
in high level.	This is not likely to be a practical limitation.
 
A typical 16 bit Forth interpreter takes 35 cycles and looks
like this:
 
	LODSW
	MOV	BX,AX
	JMP	[BX]
 
I have seen some 32 bit inner interpreter designs that take 100s
of cycles!  To be kind I won't name the authors, but there are
not that many 32 bit Forths around.
 
This inner interpreter is faster than segment tokens, mixed
length tokens, full seg:offset tokens and indirect offset tokens
to name a few.	It is even faster than using pure assembler FAR
CALL/RET instructions to implement high level code, not to
mention requiring only 2 bytes per token instead of 5.
 
Optimizations
=============
 
I carefully optimized all my Jump instructions to fall through
on the normal case.  I keep a zero in the DI register for ready
use.  CONSTANTS generate assembler code inline.  LITERALS often
generate single token references to common CONSTANTS.
 
Nearly all of the nucleus is written in code rather than high
level.	I wrote a lot of the code in several different ways then
counted cycles.  In some cases it uses different code depending
on which chip you have.
 
I make extensive use of the 8086 string handling instructions
doing things 16 bits at a time wherever possible.  For people
lucky enough to have 16 bit chips like the 80286, I carefully
arranged that most quantities and strings are word aligned.
 
I discovered a new way to do multiprecision divides that does
not require back multiplication.  Knuth isn't the last word
after all.  Divide optimizes for common special cases.
 
Register Conventions
====================
 
The 8086 is not a very orthogonal design.  Each register has
special properties that not all registers can do.  In designing
my register conventions I paid careful attention to these:
 
AX = LODSW STOSW MOV MUL DIV
BX = [BX] [BX+disp]
CX = counts LOOP and REP
DX = pairs well with AX in CWD, MUL, DIV
SI = pairs with DS: LODSW MOVSW
DI = pairs with ES: MOVSW
 
Great Teachers
==============
 
I learned a lot about Forth internals from studying Ray Duncan's
PC Forth Plus.	He even gave me commented listing of one of his
earlier versions after I peppered him with too many questions.
BBL is quite different internally from PC Forth Plus, but
externally they are quite similar.  I saw many things Ray could
not change easily -- such as register conventions and dictionary
structure, and spent a lot of time planning those features.
 
My genius friend Kent Brothers, the author of VP Planner, and I
had some long discussions on the relative merits of various
inner interpreter designs.  It was all his idea.
 
The way I get such high compile speeds is:
==========================================
 
1. multithread hashed dictionaries (including the FORTH
   vocabulary) with DISCARDable nfas.
 
2. highly optimized code for all words in the INTERPRET
   compiling loop making use of REP type string instructions in
   unlikely places. e.g. BLOCK uses REP SCANSW to search the CACHE.
 
3. My source code is heavily commented and is thus quick to
   parse.  I know of no standard compile speed benchmark.  I had to
   use samples of my own code.
 
4. When you are debugging, you get these speeds (and faster with
   LOGO mode because in many case you can avoid recompilation
   altogether), but when you are producing the final relocatable
   result you have to compile twice at different load addresses
   and use the GESPENSTER utility to compare the two images and
   from that generate the EXE header relocation information.  Thus
   the net compile speed is less than half of the gross speed.
 
| ; end of comment


