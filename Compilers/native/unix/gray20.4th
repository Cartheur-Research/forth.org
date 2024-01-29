

#--------CUT---------CUT---------CUT---------CUT--------#
#########################################################
#                                                       #
# This is a shell archive file.  To extract files:      #
#                                                       #
#    1) Make a directory for the files.                 #
#    2) Write a file, such as "file.shar", containing   #
#       this archive file into the directory.           #
#    3) Type "sh file.shar".  Do not use csh.           #
#                                                       #
#########################################################
#
#
echo Extracting CHANGES
sed 's/^Z//' >CHANGES <<\STUNKYFLUFF
ZDifferences between Release 1 and Release 2
Z
ZGray now runs on computers with memory alignment restrictions, too.
ZTile Release 2 is now supported, it needs porting to run on Tile
ZRelease 1.
ZChanged "(compile)" into "compile," (ANSI).
ZNo bugs were reported. Therefore they are still there.
STUNKYFLUFF
#
#
echo Extracting README
sed 's/^Z//' >README <<\STUNKYFLUFF
Z$Id: README,v 1.2 90/04/19 17:48:14 ertl Exp Locker: anton $
Z
ZThis is Realease 2 of Gray, a parser generator written in Forth. It
Ztakes grammars in an extended BNF syntax and generates recursive
Zdescent parsers as executable Forth code. It has no special support
Zfor error recovery.
Z
Z
ZFILES
Z
ZREADME		You are reading it
ZCHANGES		describes differences to earlier releases
Zgray.f83	parser generator source file
Zgray.doc	parser generator manual
Zelse.f83	a very tiny example grammar
Zoberon.f83	a medium-sized example grammar
Zcalc.f83	an example interpreter
Zmini.f83	an example compiler
Ztest.mini	a program for mini (computes the square)
Zgraylist.f83	example extension for gray
Ztest.f83	runs the examples
Ztest.out	output of test.f83 on TILE
Z
Z
ZPORTING
Z
ZThe current version runs on TILE Release 2, a Forth 83 written in C by
ZMikael Patel (mip@ida.liu.se) and posted on alt.sources in July 1990.
ZIt should be easy to port to other Forths:
Z
ZRemove the "#include" lines in the beginning.
ZChange the definition of ":," to suit your forth.
ZChange "bits/cell" and the subsequent cell words.
ZAdjust "alignment" (not strictly necessary).
ZIf your Forth is not direct or indirect threaded, change "compile,".
Z
ZNow you should be able to run test programs, but to make Gray usable
Zfor development you need to add code for displaying the error location
Zin the places indicated by "!!!" (There is no way to get the error
Zlocation on TILE).
Z
ZIf you want more speed, you can change a lot of multiply, divide and
Zremainder operations into shifts and ands.
Z
ZI made no effort to make the test programs portable, but I only expect
Zproblems with mini and perhaps with calc.
Z
Z
ZDEPENDENCIES
Z
ZApart from the problems mentioned in the section on porting there are
Za few other noteworthy things:
Z
ZYour Forth should support long names
ZGray needs a big return stack (e.g. ~360 cells for oberon). Adjust your
ZForth-System (On TILE change RETURNSIZE in forth.c and recompile).
Z
Z
ZCOPYRIGHT
Z
ZCopyright 1990, 1991 Martin Anton Ertl
ZThis program is distributed WITHOUT ANY WARRANTY.
ZSee gray.doc or gray.f83 for the license.
STUNKYFLUFF
#
#
echo Extracting calc.f83
sed 's/^Z//' >calc.f83 <<\STUNKYFLUFF
Z( $Id: calc.f83,v 1.1 90/04/18 14:18:57 ertl Exp Locker: anton $ )
Z( Copyright 1990 Martin Anton Ertl )
Z( This program is distributed WITHOUT ANY WARRANTY. )
Z( See gray.doc or gray.f83 for the license. )
Z( a little calculator )
Z( a usage example: )
Z( you: ? )
Z( you: 2*3-5/4= )
Z( calc: 5 )
Z
Z255 max-member ( the whole character set )
Zvariable sym
Z
Z10 stack expected
Z
Z: testsym ( set -- f )
Z dup expected push
Z sym @ member? ;
Z
Z' testsym test-vector !
Z
Z: ?syntax-error ( f -- )
Z not if
Z  empty begin
Z   expected top union
Z   expected pop
Z   expected clear? until
Z  ." expected:" ['] emit apply-to-members abort
Z endif ;
Z
Z: ?readnext ( f -- )
Z ?syntax-error
Z expected clear
Z 0 begin
Z  drop key
Z  dup 32 = not until
Z sym ! ;
Z
Z: init
Z true ?readnext ;
Z
Z: t ( -- ) ( use: t c name )
Z ( make terminal name with the token c )
Z [compile] ascii singleton ['] ?readnext terminal ;
Z
Z: x ( set1 -- set2 )
Z ( read a char from the input and include it in the set )
Z [compile] ascii singleton union ;
Z
Z( make a terminal that accepts all digits )
Zempty x 0 x 1 x 2 x 3 x 4 x 5 x 6 x 7 x 8 x 9 ' ?readnext terminal digit
Z
Zt ( "("
Zt ) ")"
Zt + "+"
Zt - "-"
Zt * "*"
Zt / "/"
Zt = "="
Z
Znonterminal expr
Z
Z(( {{ 0 }}
Z   (( {{ 10 * sym @ ascii 0 - + }} digit )) ++
Z)) <- num ( -- n )
Z
Z(( num
Z|| "(" expr ")"
Z)) <- factor ( -- n )
Z
Z(( factor (( "*" factor {{ * }}
Z          || "/" factor {{ / }}
Z          )) **
Z)) <- term ( -- n )
Z
Z(( (( term
Z   || "-" term {{ 0 swap - }} ))
Z   (( "+" term {{ + }}
Z   || "-" term {{ - }} )) **
Z)) expr rule ( -- n )
Z
Z(( {{ init }} expr "=" {{ . }} )) parser ? ( -- )
Z
STUNKYFLUFF
#
#
echo Extracting else.f83
sed 's/^Z//' >else.f83 <<\STUNKYFLUFF
Z( $Id: else.f83,v 1.1 90/04/18 14:19:20 ertl Exp Locker: anton $ )
Z( Copyright 1990 Martin Anton Ertl )
Z( This program is distributed WITHOUT ANY WARRANTY. )
Z( See gray.doc or gray.f83 for the license. )
Z( dangling else )
Z( tests if gray finds ambiguity )
Z
Z10 max-member
Z
Z: token ( adr count -- )
Z singleton ['] abort terminal ;
Z
Z0 token "if"
Z1 token "then"
Z2 token "else"
Z3 token expr
Z4 token other
Z
Znonterminal stmt
Z(( other
Z|| (( "if" expr "then" stmt (( "else" stmt )) ?? ))
Z)) stmt rule
Z
Zstmt parser test
Z
STUNKYFLUFF
#
#
echo Extracting gray.doc
sed 's/^Z//' >gray.doc <<\STUNKYFLUFF
Z$Id: gray.doc,v 1.1 90/04/18 14:28:06 ertl Exp Locker: anton $
Z
ZCopyright 1990 Martin Anton Ertl
Z
ZTERMS AND CONDITIONS FOR USE, COPYING, MODIFICATION AND DISTRIBUTION
Z
Z1. You may use this product provided that
Z   a) you DO NOT USE IT FOR MILITARY PURPOSES; and
Z   b) cause the terms of parapraph 1 to apply to any products
Z   developed using this product and make these terms known to all
Z   users of such product;
ZBy using this product, you indicate the acceptance of the terms of
Zthis paragraph.
Z
Z2. Except for the restrictions mentioned in paragraph 1, you may use
Zthe Program.
Z
Z3. You may distribute verbatim or modified copies of this program,
Zprovided that
Z   a) you keep intact all copyright notices, this license, and the notices
Z   referring to this license and to the absence of warranty; and
Z   b) you cause any work that you distribute or publish that contains the
Z   Program or part of it to be licensed to all third parties under the
Z   terms of this license. You may not impose any further restriction
Z   on the recipients exercise of the rights granted herein. Mere
Z   aggregation of another independent work with the Program or its
Z   derivative on a volume of storage or distribution medium does not
Z   bring the other work under the scope of these terms; and
Z   c) you cause the derivative to carry prominent notices saying that
Z   you changed the Program.
Z
Z4. You may distribute the Program or its derivative in intermediate,
Zobject or executable code, if you accompany it with the complete
Zmachine-readable source code.
Z
Z5. By using, modifying, copying or distributing the Program you
Zindicate your acceptance of this license and all its terms and
Zconditions.
Z
Z6. This Program is provided WITHOUT WARRANTY of any kind, either
Zexpress or implied, including, but not limited to, the implied
Zwarranties of merchantability and fitness for a particular purpose. In
Zno event, unless required by applicable law or agreed to in writing,
Zwill any copyright holder, or any other party who may modify and or
Zredistribute the Program, be liable to you for any damages, even if
Zsuch holder or other party has been advised of the possibility of such
Zdamages.
Z
ZEND OF TERMS AND CONDITIONS
Z
Z
Z
ZGRAY USERS MANUAL
Z==== ===== ======
Z
ZTo understand this manual you should know Forth and language
Zdescriptions in BNF or similar syntax notations. Apologies for my
Zclumsy use of the English language.
Z
Z
ZUSES FOR GRAY
Z
ZSyntactic Analysis of Programming Languages (parsers)
ZIt should not be hard to generate parsers for most programming
Zlanguages with Gray, but you will probably have to transform the
Zgrammar (See the chapter on grammar massaging).
Z
ZLexical Analysis (scanners)
ZAlthough it is possible to use Gray for scanning, it's probably
Zoverkill and there may be better methods.
Z
Z
ZGRAMMARS
Z
ZGrammars describe the syntax of languages. A parser generator
Ztranslates grammars into parsers that can read all valid sentences
Z(programs, if we are talking about programming languages) of the
Zlanguage. Computer Scientists also say that the grammar derives the
Zsentences. Some grammatical expressions (syntax expressions) derive
Zthe empty sentence, which is denoted by 'eps'.
Z
ZThe following table shows Gray's syntax expressions (a, b, and c
Zdenote such expressions)
Z
Zname		syntax		parses		example
Z---------------------------------------------------------------
Zconcatenation	(( a b ... ))	a, then b, ...	(( "begin" word-list "until" ))
Zalternative    (( a || b ... ))	a or b ...	(( word || number ))
Zeps		eps		eps		eps
Zoption		a ??		zero or one a	(( "else" word-list )) ??
Z*-repetition	a **		zero or more a	word **
Z+-repetition	a ++		one or more a	char ++
Znonterminal	name		See text	word-list
Zterminal	name		See text	"begin"
Zaction	       {{ forth-code }}	eps		{{ . }}
Z
ZYou need not parenthesize a concatenation in an alternative;
Z(( a b || c )) is the same as (( (( a b )) || c ))
Z
Z
ZTerminals and the I/O Interface
Z
ZThe atomic units processed by a parser are the terminal symbols. They
Zare delivered by the underlying input layer and can be as complex as
Zyou like: single characters, words, etc.
Z
ZGray and its parsers distinguish terminal symbols by their tokens (A
Ztoken is an unsigned number).
Z
ZThe input layer should read one symbol in advance to allow the parser
Zto base decisions on the token of the next symbol.
Z
ZThe Interface to the input layer consists of
Z
Z1) the variable test-vector; there you should store the execution
Zaddress (cfa) of a word (let's call it test?) with the stack effect (
Zset -- f ). test? checks, if set contains the token of the next
Zsymbol. You can use member? ( set u -- f ) to test u for membership in
Zset. If the parser generated by gray is too slow, it helps to optimize
Ztest?, which is executed at least once for every symbol read,
Z
Z2) the defining-word terminal ( set cfa -- )
Z	token singleton ' check&read terminal name 
Zdeclares the word name, which can then be used in the grammar, where
Ztoken's terminal symbol should be parsed. (Before you call singleton,
Zor any other set words, you should declare the maximum set size (and
Zthus the maximum token value) with max-member ( u -- ).) You have to
Zdefine check&read ( f -- ) which later is built into the parser and
Zcalled, when the symbol is to be parsed. At that time check&read must
Zread the next symbol. check&read also checks for syntax errors, which
Zare indicated by f being false (See the section on error handling).
Zcheck&reads for special symbols, e.g.  numbers, probably will perform
Zadditional functions, e.g. pushing the value of the number (then
Zcheck&read's stack effect is ( f -- n )).
Z
Z
ZNonterminals and Rules
Z
Z	a <- name		(1)
Zor
Z	nonterminal name	(2)
Z	a name rule		(3)
Zcan be used to define name as an abbreviation for a.
Z(1) and (3) are rules for the nonterminal name, (1) and (2) are
Zdeclarations. After its declaration name can be used instead of a.
ZThis also allows recursive definitions.
Z
Z
ZActions
Z
Zare needed to turn a simple parser into an interpreter or a compiler.
ZFor parsing they behave like eps, but when they are parsed, they
Zexecute forth code. Example: if "num" parses a number and pushes its
Zvalue, then
Z	(( num {{ . }} ))
Zparses a number and prints it.
Z
ZYou may use the parameter stack as you like; therefore you should
Zwrite stack comments for every rule. You can use the return stack, but
Zthe action should have no overall return stack effect.
Z
Z
ZParser
Z
ZYou can generate the parser called name for the syntax expression a
Zwith the defining word parser:
Z	a parser name
ZAll nonterminals have to be defined; the generation may take a while
Zif the grammar is large.
Z
Z
ZDISAMBIGUATING RULES
Z
ZGrays parsers try to predict from the next token, which expression
Zthey should parse. For some grammars this is not possible--there is an
Zambiguity. Gray generates parsers anyway, but they probably won't
Zparse every sentence of the language. In ambiguous cases the parsers
Zdecide according to the following rules:
ZIn an alternative the earlier branches have higher precedence, but an
Zeps-derivation is chosen only if no branch begins with the current
Ztoken.
Z
ZThe argument of options are rather parsed than not, but if it cannot
Zbegin with the current token, it is skipped, even if it can parse eps.
ZThis may seem unimportant, since the language remains the same, but if
Zthere are actions to be executed, the results need not be what you
Zwanted.
Z
ZThe operand of repetition is parsed as often as seems possible, but
Zagain the repetition is left, if only empty sentences can be parsed.
ZHowever, the argument of the +-repetition is parsed at least once.
Z
Z
ZWARNINGS AND ERROR MESSAGES
Z
ZMost error messages tell you where they happened (This is not true
Zfor the TILE implementation). For concatenations and alternatives the
Zposition of "||" or "))" are displayed.
Z
ZErrors while reading the grammar
Z
Zno operand
ZThere is no grammar expression between "((" and "))", "((" and "||",
Z"||" and "||", or "||" and "))". Insert eps, if you want to parse the
Zempty sentence.
Z
Zmultiple rules for nonterminal
ZThere may be only one rule for every nonterminal. Use the alternative.
Z
ZError messages while generating the parser
Z
Zno rule for nonterminal
ZA nonterminal was declared and used, but there is no rule for it.
Z
Zleft recursion 
ZThe grammar contains a left recursion , i.e. the parser could recurse
Zwithout having parsed a terminal symbol. This situation would lead
Zinto an infinite recursion. Read the chapter on left recursion
Zelimination.
Z
ZThe error message you should not see
Z
Zyou found a bug
ZIndicates a bug in Gray. See the chapter on feature reports
Z
ZWarnings
Z
Zconflict: conflict-set
Z(The conflict-set is printed as a sequence of numbers. If you want to
Zprint it in a different way, store your token-printing word (token -- )
Zin the variable print-token.) Parsers with conflicts often don't
Zunderstand the language, i.e. they cannot parse all sentences.
ZTherefore you should investigate every conflict carefully and take the
Zappropriate actions (See the chapter on left factoring).
Z
ZIf the parser has to decide (e.g. between repeating another time or
Znot), but there are tokens that both alternatives can begin with, then
Zthere's a conflict and these tokens are the conflict set.
Z	(( "a" ??  "a" ))
Zshould parse "a" and "aa", but when the parser sees "a"'s token, it
Zdoes not know, whether this is the first or the second a. Whatever
Zdecision it makes, it might be wrong.
Z
Z
ZThe other warnings are less severe; they indicate that there are
Zseveral ways to derive eps. The resulting parser parses the same
Zlanguage, but actions may be executed in a different way than you
Zintended.
Z
Zwarning: two branches may be empty
ZSeveral branches of an alternative can derive eps. The first is
Zchosen.
Z
Zwarning: unnecessary option
ZYou made an expression optional that already derives eps.
Z
Zwarning: *-repetition of optional term
ZYou *-repeated an expression that can derive eps.
Z
Z
ZMASSAGING GRAMMARS
Z
ZTo get rid of left recursions and conflicts you can change the
Zgrammar to a new one that derives the same language but does not have
Zthe problems. However, that's not always possible. I will give only
Zsimple examples, you can find algorithms in the literature (e.g.
ZAlfred V. Aho, Ravi Sethi, Jeffrey D. Ullman; Compilers. Principles,
ZTechniques nad Tools; Addison-Wesley 1986).
Z
ZLeft recursion elimination
Z
ZSimple left recursions look like this:
Z	nonterminal N
Z	(( N a || b )) N rule
ZN derives b, ba, baa, ..., and the sequence above can be replaced by
Z	(( b  a ** )) <- N
Z
ZLeft factoring
Z
ZIn
Z	(( a b || a c || d ))
Zthere is a conflict between the first and the second branch. It can be
Zresolved by postponing the decision:
Z	(( a (( b || c )) || d ))
ZOften the Situation is more complex and requires heavy transformations
Zof the grammar, which makes it hard to read and difficult to use for
Ztranslation purposes. You should investigate other ways to resolve
Zconflicts, e.g. making the scanner more powerful.
Z
Z
ZERROR HANDLING
Z
ZGray provides no special help for error handling. The simplest way is
Zto print a meaningful error message, clean up and abort. One
Zpossibility for meaningful messages is printing the set of symbols
Zthat the parser expected. This set is the union of all the sets tested
Zwith test? since reading the latest terminal-symbol. See calc.f83 for
Zan example.
Z
ZA technique that allows the parser to continue when it encounters
Zcommon errors are error rules (error productions). You extend the
Zgrammar to allow the parser to parse sentences with common errors.
ZWhen parsing an error the parser should print an error message.
ZExample: Statements in Pascal are separated by semicolons:
Z	(( statement (( ";" statement )) ** )) <- StatementSequence
ZThis semicolon is often forgotten. If you don't want the compiler to
Zabort just because of a missing ";", change the rule to
Z	(( statement
Z	   (( (( ";" || {{ pascal-error ." ; inserted" }} )) statement ))
Z	)) <- StatementSequence
Z
ZSee the literature for other error recovery techniques.
Z
Z
ZTHE GUTS OF GRAY
Z
ZAs Forth programmer you will want to change or extend Gray. Here's a
Zsmall overview to make it easier.
Z
ZWhen reading the grammar words like "terminal", "??", "<-" and "))"
Zbuild an abstract syntax graph (ASG) of the grammar in memory. For
Zmost grammar constructions one node is generated; to make later work
Zeasier concatenation and alternative are translated into n-1 binary
Znodes using the binary operators "concat" and "alt".
Z
Z"parser" generates the parser in two passes: "propagate" just computes
Zthe follow sets (the follow set of an expression contains the tokens
Zof the terminal symbol that can follow the expression. Follow sets are
Zonly needed for recognizing conflicts). "pass2" computes the necessary
Zfirst sets, detects errors and warnings and generates code for all
Zrules (a first set contains the tokens of the terminals a grammar
Zexpression can begin with. If the expression can derive eps, the
Zfirst set also contains eps. Since Grays sets can only contain tokens,
Zepsilon-derivations are indicated by the extra flag maybe-empty).
ZFinally, "parser" generates code for the start expression (the operand
Zof "parser").
Z
ZThere are small subpasses for first set computation ("compute") and
Zcode generation ("generate"), that walk over the parts of the ASG that
Zthey need. To save a lot of computation every ASG-node memoizes the
Zresult of "compute". "compute" also detects left recursions and
Zwarnings: A nonterminal comes up twice in a computation, iff there is
Za left recursion. Therefore, to detect all left recursions, "compute"
Zis called for every node.
Z
ZTo avoid obscure swap-drop-flip-flop-orgies I used a context stack (It
Zhas nothing to do with forth's "context").  The ASG-node of the
Zcurrent expression is on top of this stack and can be accessed with
Z"this". You can access the fields of this node just by mentioning
Ztheir names. This implies that for accessing such fields you have to
Zpush the node on the context-stack first.
Z
ZAnother programming technique used in Gray are methods and maps.
Z"compute", "generate", "propagate" and "pass2" have general functions
Zas well as functions specific to the node type. The special code is
Zcalled via an execution address table (map) that the "methods" field
Zof "this" points to. Words defined with "method" automagically execute
Zthis calling procedure.
Z
ZCode and data structure sharing impose a class hierarchy on the node
Ztypes:
Z
Zsyntax-expr
Z	terminal
Z	eps
Z	nt (nonterminal)
Z	action
Z	unary
Z		option&repetition
Z			option
Z			repetition
Z				*repetition
Z				+repetition
Z	binary
Z		concatenation
Z		alternative
Z
Z
ZGLOSSARY
Zdoesn't contain everything
Z
ZWords for building grammars
ZWords shown in the table in the "grammars" chapter are not shown here.
Z
Zconcat	( syntax-expr1 syntax-expr2 -- syntax-expr )
Zalt	( syntax-expr1 syntax-expr2 -- syntax-expr )
Z	binary postfix operators for concatenation and alternative
Z(-,-)	( use: (- syntax-expr1 ... -);  -- syntax-expr )
Z	another concatenation notation; same as (( syntax-expr1 ... ))
Z(|,|)	( use: (| syntax-expr1 ... |);  -- syntax-expr )
Z	another alternative notation; same as (( syntax-expr1 || ... ))
Zterminal ( use: terminal name; set check&read -- )
Z	defines name ( -- syntax-expr ) as terminal with first-set
Z	"set" and parse-time action check&read ( f -- ). See section
Z	"Terminals and the I/O Interface". 
Znonterminal ( use: nonterminal name;  -- )
Z	declares name ( -- syntax-expr ) as nonterminal. See section
Z	"Nonterminals and Rules".
Zrule	( syntax-expr nt -- )
Z	makes "nt" an abbreviation for "syntax-expr".
Z<-	( use: <- name; syntax-expr -- )
Z	defines "name" ( -- syntax-expr2 ) as abbreviation for
Z	"syntax-expr".
Z
ZWords necessary for parser generation
Z
Zmax-member ( u -- )
Z	declares u to be the maximum member of sets generated
Z	later. Must be called before using any set word except
Z	"member?" and thus before building a grammar.
Ztest-vector ( a variable initially containing ' abort )
Z	before you call "parser", you should store into test-vector
Z	the execution address of a word ( set -- f ) that returns true
Z	if the token of the current symbol is in "set". 
Zparser	( use: parser name; syntax-expr -- )
Z	generates a parser for syntax-expr that you can call by
Z	"name".
Z
ZSet words
Z
Zmax-member ( u -- )
Z	declares u to be the maximum member of sets generated
Z	later. Must be called before using any set word except
Z	"member?".
Zempty	( -- set )
Z	the empty set of the current size.
Zsingleton ( u -- set )
Z	makes a set that contains u and nothing else
Zunion	( set1 set2 -- set )
Zintersection ( set1 set2 -- set )
Z	set operations
Zmember?	( set u -- f )
Z	returns true if u is in set
Zsubset?	( set1 set2 -- f )
Z	returns true if every member of set1 is in set2
Zdisjoint? ( set1 set2 -- f )
Z	returns true if set1 and set2 heve no common members
Zapply-to-members ( set [ u -- ] -- )
Z	executes [ u -- ] for every member of set
Z
ZCompilation words
Z
Z:,	( -- )
Z	creates anonymous colon definition header
Zcompile, ( execution-addr -- )
Z	compiles the execution address, e.g.  ' word compile,  is the
Z	same as  compile word
Zcompile-test ( set -- )
Z	compiles a test for "set" using "test-vector"
Z
ZContext Management
Z
Znew-context ( syntax-expr -- )
Zold-context ( -- )
Z	push and pop respectively
Zthis	( -- syntax-expr )
Z	the current syntax-expr, i.e. top of context-stack
Z
ZWarnings and Errors
Z
Z.in	( -- )
Z	print source location of "this", i.e. where the error
Z	happened.
Zgray-error ( -- )
Z	prints the source location and aborts
Zcheck-conflict ( set1 set2 -- )
Z	prints a warning if set1 and set2 conflict (are not disjoint)
Z	
ZSyntax Expression Operations
ZYou have to substitute a class name for ... in the following words.
Z
Zmake-syntax-expr ( map -- syntax-expr )
Zmake-terminal ( first-set execution-addr -- syntax-expr )
Zmake-binary ( syntax-expr1 syntax-expr2 map -- syntax-expr )
Zmake-unary ( syntax-expr1 map -- syntax-expr2 )
Zmake-nt ( syntax-expr -- nt )
Zconcat, alt, ??, ++, **, etc.
Z	allocate an ASG node and initialize it. "make-terminal" and
Z	"make-nt" are anonymous versions of the defining words.
Z
Zcompute	( syntax-expr -- first-set maybe-empty )
Z	compute the first-set and maybe-empty of syntax-expr
Zget-first ( syntax-expr -- first-set )
Z	compute just the first set of syntax-expr
Zcheck-cycle ( syntax-expr -- )
Z	just check for left recursion
Zpropagate ( follow-set syntax-expr -- )
Z	add "follow-set" to the follow set of "syntax-expr" and its
Z	children
Zgenerate ( syntax-expr -- )
Z	generate code for "syntax-expr"
Zpass2	( syntax-expr -- )
Z	computes all necessary first sets, checks for left recursions
Z	and conflicts and generates code for rules
Z...-syntax-expr ( -- n )
Z	a constant containing the length of a ... ASG node
Z...-map	( a "create"d word )
Z	contains the method pointers for ...
Zcompute-... ( -- first maybe-empty )
Zpropagate-... ( follow-set -- )
Zgenerate-... ( -- )
Zpass2-... ( -- )
Z	execute the ...-specific part of compute, propagate, generate
Z	and pass2. The syntax-expr treated is "this".
Z
Z
ZAN EXAMPLE OF AN EXTENSION
Z
ZIn Pascal and similar languages there are many expressions of the type
Z	(( a  (( b a )) ** ))
ZLet's call them lists. The experienced programmer will immediately
Zfactor out the common things and introduce a new operator: &&, as in
Z	a b &&
ZYou can define this operator to be just an abbreviation:
Z	: && ( syntax-expr1 syntax-expr2 -> syntax-expr3 )
Z		over concat ** concat ;
Z(I use concat here since the parenthesized notation needs more stack
Zmanipulation.)
ZWhen you use this operator, two pointers to syntax-expr1 are generated.
ZThis is OK. Cycles, however, must contain nonterminals to avoid
Zinfinite recursions in generate.
ZThe definition of && is good enough for nearly everything, but for the
Zsake of the example, let's do a version that generates
Z    begin [ a generate ] ... test? while [ b generate ] repeat
Zinstead of
Z    [ a generate ] begin ... test? while [ b generate a generate ] repeat
ZYou find the program described here in graylist.f83.
Z"&&" makes a binary node with an additional field that is explained
Zlater. Its map points to list-specific words that we have to define
Znow:
Z
Z"generate-list" is the easiest, since we know already what it should
Zdo. The only thing unknown is the set, that is to be tested: There
Zshould be another repetition, if the next token is in the first set of
Z( b a ). Thus, the set to be tested is the first set of b; if b can
Zderive epsilon, b is transparent and the first-set of a has to be
Zadded. Since no memory may be allocated while "generate"ing, set
Zoperations like "union" are forbidden. Therefore the set is computed
Zin pass2-list and stored in the field "test-set".
Z
ZThe next task is "compute-list". If a cannot derive epsilon, the first
Zset of the expression is the first set of a. If a can derive epsilon,
Zthe expression can begin with b and b's first set has to be added. The
Zexpression derives epsilon, if a derives epsilon.
Z
Z"propagate-list" is quite different from "compute-list": The followset
Zis passed in, and it must pass the follow-sets to a and b. If no
Zoperand derives epsilon, the follow set of b is the first set of a and
Zthe follow set of a is the first set of b united with the follow set
Zof the whole expression. If an operand derives epsilon, its follow set
Zshines through and must be added to the follow set of the other
Zoperand.
Z
Z"pass2-list" has to recognize conflicts, compute test-set and call
Z"pass2" for the operands. The latter task is the same for all binary
Znodes and is performed by pass2-binary. There's a conflict, if a
Zdecision has to be made and there are tokens that both alternatives
Zbegin with. In our case the parser has to decide between another
Zrepetition or ceasing to parse the expression. The sets of tokens that
Zthese alternatives begin with are the test-set and the follow set of
Zthe expression respectively. If these sets are not disjoint, then
Zthere is a conflict and the intersection of these sets is the conflict
Zset.
Z
Z
ZKNOWN FEATURES
Z
ZAs usual in Forth, syntax error checking is minimal. You can find some
Zerrors (e.g. missing parenthesis) by checking the stack for forgotten
Zcells.
ZWarnings and error messages are printed even if the problem cannot
Zshow up due to the disambiguating rules.
Z
Z
ZFEATURE REPORTS
Z
ZIf you find a new feature, mail a report to ertl@vip.at.
ZIn this report you should describe the behaviour that constitutes the
Zfeature and how to reproduce this behaviour (A program would be nice).
Z
Z
ZAUTHOR
Z
ZM. Anton Ertl
ZWiedner Hauptstrasse 141/1/7
ZA-1050 WIEN
ZAUSTRIA
ZEmail:	ertl@vip.at
Z	...!mcsun!tuvie!vip!ertl
ZIf you find Gray useful, you might want to send me a contribution.
ZSend it by international money order (to my address, or to the account
Zno. 1100167 of Leopoldine Ertl at the "Oesterreichische
ZPostsparkasse") or in cash, foreign checks cost me $7-$8 to convert
Zinto cash.
Z
Z
ZUPDATES
Z
ZIf you mail me your e-address, I will e-mail you updates, when they
Zappear.
Z
Z
ZRELATED WORK
Z
ZMikael Patel (mip@ida.liu.se) sent me a parser generator (or rather
Zparser interpreter), that's based upon top-down parsing with
Zbacktracking. The code seemed quite TILE-specific, so it's probably
Zhard to port.
Z
ZIn "A User Definable Language Interface for Forth" (Journal of Forth
ZApplication and Research (JFAR) 6/1 (1990)) Tyler A. Ivanco and
ZGeoffry Hunter describe two parser generators, both based upon
Ztop-down-parsing: the first one seems to be similar to gray, i.e. it
Zgenerates a recursive descent parser. The other one consists of two
Zparts: A Pascal program that generates a parsing table, and a Forth
Zprogram that interprets it. You can reach the authors at
ZFS3200022@sol.yocku.ca or tyler@stpl.ists.ca.
STUNKYFLUFF
#
#
echo Extracting gray.f83
sed 's/^Z//' >gray.f83 <<\STUNKYFLUFF
Z\ $Id: gray.f83,v 1.1 90/04/18 14:19:59 ertl Exp Locker: anton $ )
Z\ Copyright 1990 Martin Anton Ertl
Z\
Z\ TERMS AND CONDITIONS FOR USE, COPYING, MODIFICATION AND DISTRIBUTION
Z\ 
Z\ 1. You may use this product provided that
Z\    a) you DO NOT USE IT FOR MILITARY PURPOSES; and
Z\    b) cause the terms of parapraph 1 to apply to any products
Z\    developed using this product and make these terms known to all
Z\    users of such product;
Z\ By using this product, you indicate the acceptance of the terms of
Z\ this paragraph.
Z\ 
Z\ 2. Except for the restrictions mentioned in paragraph 1, you may use
Z\ the Program.
Z\ 
Z\ 3. You may distribute verbatim or modified copies of this program,
Z\ provided that
Z\    a) you keep intact all copyright notices, this license, and the notices
Z\    referring to this license and to the absence of warranty; and
Z\    b) you cause any work that you distribute or publish that contains the
Z\    Program or part of it to be licensed to all third parties under the
Z\    terms of this license. You may not impose any further restriction
Z\    on the recipients exercise of the rights granted herein. Mere
Z\    aggregation of another independent work with the Program or its
Z\    derivative on a volume of storage or distribution medium does not
Z\    bring the other work under the scope of these terms; and
Z\    c) you cause the derivative to carry prominent notices saying that
Z\    you changed the Program.
Z\ 
Z\ 4. You may distribute the Program or its derivative in intermediate,
Z\ object or executable code, if you accompany it with the complete
Z\ machine-readable source code.
Z\ 
Z\ 5. By using, modifying, copying or distributing the Program you
Z\ indicate your acceptance of this license and all its terms and
Z\ conditions.
Z\ 
Z\ 6. This Program is provided WITHOUT WARRANTY of any kind, either
Z\ express or implied, including, but not limited to, the implied
Z\ warranties of merchantability and fitness for a particular purpose. In
Z\ no event, unless required by applicable law or agreed to in writing,
Z\ will any copyright holder, or any other party who may modify and or
Z\ redistribute the Program, be liable to you for any damages, even if
Z\ such holder or other party has been advised of the possibility of such
Z\ damages.
Z\ END OF TERMS AND CONDITIONS )
Z
Z\ recursive descent parser generator )
Z
Z\ !! tile only )
Z#include internals.f83
Z#include structures.f83
Z
Z.( Loading gray ... Copyright 1990 Martin Anton Ertl; NO WARRANTY ) cr
Z
Z\ misc )
Z: noop ;
Z
Z4 constant cell \ !! implementation dependent )
Z: cell+  cell + ;
Z: cells  cell * ;
Zcell 8 * constant bits/cell \ !! implementation dependent )
Z
Zcell constant alignment \ !! implementation dependent )
Z: align \ addr1 -- addr2 )
Z\ makes aligned addr2 from addr1 )
Z 1- alignment + alignment / alignment * ;
Z: alignallot \ -- )
Z\ aligns here )
Z here align here - allot ;
Z
Z\ for fig-forth )
Z\ : create \ use: create word )
Z\          \ word: -- adr )
Z\  0 variable -1 cells allot ;
Z
Z: c, \ c -- )
Z here 1 allot c! ;
Z
Z\ : 2@ \ addr -- n1 n2 )
Z\  dup cell+ @ swap @ ;
Z
Z\ : 2! \ n1 n2 addr -- )
Z\  swap over ! cell+ ! ;
Z
Z\ : 2, \ n1 n2 -- )
Z \  here 2 cells allot 2! ;
Z
Z: 2dup over over ;
Z: 2drop drop drop ;
Z
Z: rdrop r> r> drop >r ;
Z
Z: endif [compile] then ; immediate
Z
Z: ?pairs ( n1 n2 -- )
Z ( aborts, if the numbers are not equal )
Z = not if
Z  abort
Z endif ;
Z 
Z: ', \ -- ) ( use: ', name )
Z ' , ;
Z
Z0 constant false
Zfalse not constant true
Z
Z: :, \ -- ) ( creates anonymous colon def header )
Z\ !! implementation dependent )
Z\ ( for sane forths: )
Z\  [ ' noop @ ] literal , ;
Z\ for tile )
Z [ structures as ENTRY ] literal make [ forth only ]
Z COLON over +code !
Z here swap +parameter ! ;
Z
Z: compile, \ execution-token -- )
Z\ compiles the exec-token, e.g. ' word compile,  is the same as compile word ) 
Z\ !! implementation dependent )
Z , ;
Z\ here's an inefficient version that should work with all forths: )
Z\ [compile] literal compile execute ; )
Z
Z
Z\ stack administration )
Z\ this implementation is completely unsafe )
Z
Z: stack \ n -- )
Z\ use: n stack word )
Z\ creates a stack called word with n cells )
Z\ the first cell is the stackpointer )
Z create here , cells allot ;
Z
Z: push \ n stack -- )
Z cell over +! @ ! ;
Z
Z: top \ stack -- n )
Z @ @ ;
Z
Z: pop \ stack -- )
Z [ -1 cells ] literal swap +! ;
Z
Z: clear? \ stack -- f )
Z dup @ = ;
Z
Z: clear \ stack -- )
Z dup ! ;
Z
Z
Z\ sets - represented as bit arrays )
Z\ bits that represent no elements, must be 0 )
Z\ all operations assume valid parameters )
Z\ emements must be unsigned numbers )
Z\ the max. element size must be declared with max-member )
Z\ no checking is performed )
Z\ set operations allot memory )
Z
Zcreate bit-table bits/cell cells allot
Z\ this table contains a mask for every bit in a cell )
Z: init-bit-table \ -- )
Z 1 bits/cell 0 do
Z  dup  bit-table i cells + !
Z  dup +
Z loop
Z drop ;
Zinit-bit-table forget init-bit-table
Z
Z: decode \ u -- w )
Z\ returns a cell with bit# u set and everyting else clear )
Z cells bit-table + @ ;
Z
Zvariable cells/set 0 cells/set !
Zvariable empty-ptr 0 empty-ptr ! \ updatd by max-member )
Z: empty \ -- set )
Z empty-ptr @ ;
Z
Z: max-member \ u -- )
Z\ declares u to be the maximum member of sets generated afterwards )
Z\ must be called before using any set word except member?, add-member )
Z bits/cell / 1+
Z dup cells/set !
Z here empty-ptr ! \ make empty set )
Z 0 do 0 , loop ;
Z
Z: copy-set \ set1 -- set2 )
Z\ makes a copy of set1 )
Z here swap
Z cells/set @ 0 do
Z  dup @ ,
Z  cell+ loop
Z drop ;
Z
Z: normalize-bit-addr \ addr1 u1 -- addr2 u2 )
Z\ addr1*bits/cell+u1=addr2*bits/cell+u2, u2<bits/cell )
Z bits/cell /mod
Z cells rot +
Z swap ;
Z
Z: add-member \ u set -- )
Z\ changes set to include u )
Z swap normalize-bit-addr
Z decode
Z over @ or swap ! ;
Z
Z: singleton \ u -- set )
Z\ makes a set that contains u and nothing else )
Z empty copy-set swap over add-member ;
Z
Z: member? \ set u -- f )
Z\ returns true if u is in set )
Z normalize-bit-addr
Z decode
Z swap @ and
Z 0= not ;
Z
Z: binary-set-operation \ set1 set2 [w1 w2 -- w3] -- set )
Z\ creates set from set1 and set2 by applying [w1 w2 -- w3] on members )
Z\ e.g. ' or binary-set-operation  is the union operation )
Z here >r
Z cells/set @ 0 do >r
Z  over @ over @ r@ execute ,
Z  cell+ swap cell+ swap
Z r> loop
Z drop 2drop r> ;
Z
Z: union \ set1 set2 -- set )
Z ['] or binary-set-operation ;
Z
Z: intersection \ set1 set2 -- set )
Z ['] and binary-set-operation ;
Z
Z: binary-set-test? \ set1 set2 [w1 w2 -- w3] -- f )
Z\ returns true, if [w1 w2 -- w3] binary-set-operation returns empty )
Z\ e.g. set1 set2 ' and binary-set-test?  is true, if set1 and set2
Z\ are disjoint, i.e. they contain no common members )
Z >r true rot rot r>
Z cells/set @ 0 do >r
Z  over @ over @ r@ execute 0= not if
Z   rot drop false rot rot
Z  endif
Z  cell+ swap cell+ swap
Z r> loop
Z drop 2drop ;
Z
Z: notb&and \ w1 w2 -- w3 )
Z -1 xor and ;
Z
Z: subset? \ set1 set2 -- f )
Z\ returns true if every member of set1 is in set2 )
Z ['] notb&and binary-set-test? ;
Z
Z: disjoint? \ set1 set2 -- f )
Z\ returns true if set1 and set2 heve no common members )
Z ['] and binary-set-test? ;
Z
Z: apply-to-members \ set [ u -- ] -- )
Z\ executes [ u -- ] for every member of set )
Z cells/set @ bits/cell * 0 do
Z  over i member? if
Z   i over execute
Z  endif
Z loop
Z 2drop ;
Z
Z: union \ set1 set2 -- set )
Z\ just a little more space-efficient ) 
Z 2dup subset? if
Z  swap drop
Z else 2dup swap subset? if
Z  drop
Z else
Z  union
Z endif endif ;
Z
Z
Z\ tests )
Zvariable test-vector ' abort test-vector !
Z\ here you should store the execution address of a word ( set -- f )
Z\ that returns true if the token of the current symbol is in set )
Z
Z: compile-test \ set -- )
Z [compile] literal
Z test-vector @ compile, ;
Z
Z
Z\ context management )
Z500 stack context-stack
Z\ this stack holds the syntax-exprs currently being treated )
Z\ enlarge it, if your grammar is large and complex )
Zcontext-stack clear
Z
Z: this \ -- syntax-expr )
Z\ get current syntax-expr )
Z context-stack top ;
Z
Z: new-context \ syntax-expr -- )
Z context-stack push ;
Z
Z: old-context \ -- )
Z context-stack pop ;
Z
Z
Z\ structures )
Z: <builds-field \ n1 n2 -- n3 ) ( defining-word )
Z\ n1 is the offset of the field, n2 its length, n3 the offset of the
Z\ next field; creates a word that contains the offset )
Z create over , + ;
Z
Z0 constant struct
Z\ initial offset
Z
Z: context-var \ use: < offset > size context-var name < offset2 > )
Z\ name returns the address of the offset field of "this" )
Z <builds-field \ n1 n2 -- n3 )
Z does> \ -- addr )
Z  @ this + ;
Z
Z: context-const \ use: < offset > context-const name < offset2 > )
Z\ name returns the contents of the field of this at offset )
Z cell <builds-field \ n1 -- n2 )
Z does> \ -- n )
Z  @ this + @ ;
Z
Z
Z\ syntax-exprs )
Zstruct
Z align context-const methods
Z        \ table of words applicable to the syntax-expr (a map)
Z 1 context-var mark-propagate \ used to ensure that "propagate" is
Z        \ called at least once for each syntax-expr )
Z 1 context-var mark-pass2
Z        \ make sure pass2 is called exactly once )
Z align cell context-var first-set
Z        \ all tokens a nonempty path may begin with )
Z        \ if it's equal to 0, the first-set has not been computed yet )
Z 1 context-var maybe-empty
Z        \ true if the syntax-expr can derive eps )
Z align cell context-var follow-set
Z	\ the tokens of the terminals that can follow the syntax-expr )
Z\ !!! align 2 cells context-var source-location \ for error msgs )
Zconstant syntax-expr   \ length of a syntax-expr )
Z
Z: make-syntax-expr \ map -- syntax-expr )
Z\ allocate a syntax-expr and initialize it )
Z here swap , false c, false c,
Z alignallot 0 , false c, alignallot empty ,
Z\ !!! in @ line# @ 2, \ store source location )
Z ;
Z
Z
Z\ warnings and errors )
Z: .in \ -- )
Z\ prints where the error happened )
Z\ !!! source-location 2@ ."  in line " . ." column " . ;
Z ;
Z 
Z: gray-error .in abort ;
Z
Z: internal-error
Z cr ." you found a bug" gray-error ;
Z
Zvariable print-token ' . print-token !
Z\ contains execution address of a word < token -- > to print a token )
Z
Z: check-conflict \ set1 set2 -- )
Z\ print the intersection of set1 and set2 if it isn't empty )
Z 2dup disjoint? not if
Z  cr ." conflict:"
Z  intersection print-token @ apply-to-members
Z  .in
Z else
Z  2drop
Z endif ;
Z
Z
Z\ methods and maps )
Z: method \ use: < offset > method name < offset2 > )
Z\ executes the word whose execution address is stored in the field
Z\ at offset of a table pointed to by the "methods" field of "this" ) 
Z cell <builds-field \ n1 -- n2 )
Z does>
Z  @ methods + @ execute ;
Z
Z\ method table for syntax-exprs
Zstruct
Z method compute-method
Z method propagate-method
Z method generate-method
Z method pass2-method
Zconstant syntax-expr-methods
Z
Z
Z\ general routines )
Z: compute \ syntax-expr -- first-set maybe-empty )
Z\ compute the first-set and maybe-empty of a syntax-expr )
Z\ a bit of memoization is used here )
Z new-context
Z first-set @ 0= if
Z  compute-method
Z  maybe-empty c!
Z  first-set !
Z endif
Z first-set @ maybe-empty c@
Z old-context ;
Z
Z: get-first \ syntax-expr -- first-set )
Z compute drop ;
Z
Z: check-cycle \ syntax-expr -- )
Z\ just check for left recursion )
Z compute 2drop ;
Z
Z: propagate \ follow-set syntax-expr -- )
Z\ add follow-set to the follow set of syntax-expr and its children ) 
Z new-context
Z dup follow-set @ subset? not  \ would everything stay the same
Z mark-propagate c@ not or if   \ and was propagate here already
Z  true mark-propagate c!       \ NO, do propagate
Z  follow-set @ union dup follow-set !
Z  propagate-method
Z else
Z  drop
Z endif
Z old-context ;
Z
Z: generate \ syntax-expr -- )
Z\ this one gets things done )
Z new-context generate-method old-context ;
Z
Z: pass2 \ syntax-expr -- )
Z\ computes all necessary first sets, checks for left recursions
Z\ and conflicts and generates code for rules )
Z new-context
Z mark-pass2 c@ not if
Z  true mark-pass2 c!
Z  this check-cycle
Z  pass2-method
Z endif
Z old-context ;
Z
Z
Z\ main routine )
Z: parser \ syntax-expr -- )
Z\ use: syntax-expr parser xxx )
Z context-stack clear
Z empty over propagate
Z dup pass2
Z >r [compile] : r> generate [compile] ; ;
Z
Z
Z\ eps - empty syntax-expr )
Zcreate eps-map
Z', internal-error
Z', drop
Z', noop
Z', noop
Z
Zcreate eps1
Z\ the eps syntax-expr proper
Z eps-map make-syntax-expr
Zdrop
Z
Z: eps \ -- syntax-expr )
Z\ just adjusts eps1 and returns it
Z eps1 new-context
Z empty first-set ! ( empty changes due to max-member )
Z true maybe-empty c!
Z old-context
Z eps1 ;
Z
Z\ terminals )
Z\ a terminal is a syntax-expr with an extra field )
Zsyntax-expr
Z context-const check&next
Z        \ contains address of a word < f -- > that checks
Z        \ if f is true and reads the next terminal symbol )
Zconstant terminal-syntax-expr
Z
Z: generate-terminal \ -- )
Z this get-first compile-test
Z check&next compile, ;
Z
Zcreate terminal-map
Z', internal-error
Z', drop
Z', generate-terminal
Z', noop
Z
Z: make-terminal \ first-set cfa -- syntax-expr )
Z terminal-map make-syntax-expr
Z new-context
Z ,
Z first-set !
Z this old-context ;
Z
Z: terminal \ first-set cfa -- )
Z create make-terminal drop ;
Z
Z
Z\ binary syntax-exprs )
Zsyntax-expr
Z context-const operand1
Z context-const operand2
Zconstant binary-syntax-expr
Z
Z: make-binary \ syntax-expr1 syntax-expr2 map -- syntax-expr )
Z make-syntax-expr rot , swap , ;
Z
Z: pass2-binary
Z operand1 pass2
Z operand2 pass2 ;
Z
Z
Z\ concatenations )
Z: compute-concatenation \ -- first maybe-empty )
Z operand1 compute dup if
Z  drop
Z  operand2 compute
Z  >r union r>
Z endif ;
Z
Z: propagate-concatenation \ follow-set -- )
Z operand2 compute if
Z  over union
Z endif \ follow follow1 )
Z operand1 propagate
Z operand2 propagate ;
Z
Z: generate-concatenation \ -- )
Z operand1 generate
Z operand2 generate ;
Z
Zcreate concatenation-map
Z', compute-concatenation
Z', propagate-concatenation
Z', generate-concatenation
Z', pass2-binary
Z
Z: concat \ syntax-expr1 syntax-expr2 -- syntax-expr )
Z concatenation-map make-binary ;
Z\ this is the actual concatenation operator )
Z\ but for safety and readability the parenthesised notation )
Z\ is preferred )
Z
Z
Z\ alternatives )
Z: compute-alternative \ -- first maybe-empty )
Z operand1 compute
Z operand2 compute
Z rot 2dup and if
Z  cr ." warning: two branches may be empty" .in endif
Z or >r union r> ;
Z
Z: propagate-alternative \ follow -- )
Z dup operand1 propagate
Z operand2 propagate ;
Z
Z: generate-alternative1 \ -- )
Z operand1 get-first compile-test
Z [compile] if
Z operand1 generate
Z [compile] else
Z operand2 generate
Z [compile] endif ;
Z
Z: generate-alternative2 \ -- )
Z operand1 get-first compile-test compile not
Z operand2 get-first compile-test compile and
Z [compile] if
Z operand2 generate
Z [compile] else
Z operand1 generate
Z [compile] endif ;
Z
Z: generate-alternative \ -- )
Z operand1 compute if
Z  generate-alternative2
Z else
Z  generate-alternative1
Z endif
Z drop ;
Z
Z: pass2-alternative \ -- )
Z this compute if
Z  follow-set @ check-conflict
Z else
Z  drop
Z endif
Z operand1 get-first operand2 get-first check-conflict
Z pass2-binary ;
Z
Zcreate alternative-map
Z', compute-alternative
Z', propagate-alternative
Z', generate-alternative
Z', pass2-alternative
Z
Z: alt \ syntax-expr1 syntax-expr2 -- syntax-expr )
Z alternative-map make-binary ;
Z\ this is the actual alternative operator )
Z\ but for safety and readability the parenthesised notation )
Z\ is preferred )
Z
Z
Z\ unary syntax-exprs )
Zsyntax-expr
Z context-const operand
Zconstant unary-syntax-expr
Z
Z: make-unary \ syntax-expr1 map -- syntax-expr2 )
Z make-syntax-expr swap , ;
Z
Z
Z\ options and repetitions )
Z: pass2-option&repetition \ -- )
Z follow-set @ operand get-first check-conflict
Z operand pass2 ;
Z
Z
Z\ options )
Z: compute-option \ -- set f )
Z operand compute if
Z  cr ." warning: unnessesary option" .in endif
Z true ;
Z
Z: propagate-option \ follow -- )
Z operand propagate ;
Z
Z: generate-option \ -- )
Z operand get-first compile-test
Z [compile] if
Z operand generate
Z [compile] endif ;
Z
Zcreate option-map
Z', compute-option
Z', propagate-option
Z', generate-option
Z', pass2-option&repetition
Z
Z: ?? \ syntax-expr1 -- syntax-expr2 )
Z option-map make-unary ;
Z
Z
Z\ repetitions )
Z: propagate-repetition \ follow-set -- )
Z operand get-first union operand propagate ;
Z
Z
Z\ *-repetitions )
Z: compute-*repetition \ -- set f )
Z operand compute if
Z  cr ." warning: *repetition of optional term" .in endif
Z true ;
Z
Z: generate-*repetition \ -- )
Z [compile] begin
Z operand get-first compile-test
Z [compile] while
Z operand generate
Z [compile] repeat ;
Z
Zcreate *repetition-map
Z', compute-*repetition
Z', propagate-repetition
Z', generate-*repetition
Z', pass2-option&repetition
Z
Z: ** \ syntax-expr1 -- syntax-expr2 )
Z *repetition-map make-unary ;
Z
Z
Z\ +-repetitions )
Z: compute-+repetition \ -- set f )
Z operand compute ;
Z
Z: generate-+repetition \ -- )
Z [compile] begin
Z operand generate
Z operand get-first compile-test
Z compile not [compile] until ;
Z
Zcreate +repetition-map
Z', compute-+repetition
Z', propagate-repetition
Z', generate-+repetition
Z', pass2-option&repetition
Z
Z: ++ \ syntax-expr1 -- syntax-expr2 )
Z +repetition-map make-unary ;
Z
Z
Z\ actions )
Zsyntax-expr
Z 0 context-var action
Zconstant action-syntax-expr
Z
Z: generate-action \ syntax-expr -- )
Z action compile, ;
Z
Zcreate action-map
Z', internal-error
Z', drop
Z', generate-action
Z', noop
Z
Z: {{ \ -- syntax-expr )
Z action-map make-syntax-expr
Z new-context
Z empty first-set !
Z true maybe-empty c!
Z this old-context
Z \ ?exec !csp )
Z ] :, ;
Z
Z: }} \ syntax-expr -- syntax-expr )
Z \ ?csp )
Z compile exit
Z [compile] [
Z; immediate
Z
Z
Z\ nonterminals )
Zsyntax-expr
Z 1 context-var mark-compute
Z align cell context-var rule-body \ in forth left side of rule )
Z cell context-var exec            \ cfa of code for rule )
Zconstant nt-syntax-expr
Z
Z: get-body \ -- syntax-expr )
Z\ get the body of the rule for the nt in "this" )
Z  rule-body @ if
Z   rule-body @
Z  else
Z   cr ." no rule for nonterminal" gray-error
Z  endif ;
Z
Z: compute-nt \ -- set f )
Z mark-compute c@ if
Z  cr ." left recursion" gray-error
Z else
Z  true mark-compute c!
Z  get-body compute
Z endif ;
Z
Z: propagate-nt \ follow-set -- )
Z  get-body propagate ;
Z
Z: code-nt \ -- )
Z\ generates the code for a rule )
Z here exec !
Z ] :,
Z get-body generate
Z compile exit [compile] [ ;
Z
Z: generate-nt \ -- )
Z\ generates a call to the code for the rule )
Z\ since the code needs not be generated yet, an indirect call is used )
Z exec [compile] literal
Z compile @
Z compile execute ;
Z
Z: pass2-nt \ -- )
Z\ apart from the usual duties, this pass2 also has to code-nt )
Z get-body pass2
Z code-nt ;
Z
Zcreate nt-map
Z', compute-nt
Z', propagate-nt
Z', generate-nt
Z', pass2-nt
Z
Z: make-nt \ syntax-expr -- nt )
Z nt-map make-syntax-expr
Z false c, alignallot swap , 0 , ;
Z
Z: <- \ use: syntax-expr <- xxx )
Z     \ xxx: -- syntax-expr )
Z create make-nt drop ;
Z
Z: nonterminal \ use: nonterminal xxx )
Z 0 <- ;       \ forward declaration )
Z
Z: rule \ syntax-expr nt -- )
Z\ makes a rule )
Z new-context
Z rule-body @ if
Z  ." multiple rules for nonterminal" gray-error endif
Z rule-body !
Z old-context ;
Z
Z
Z\ syntactic sugar )
Z: reduce \ 0 x1 ... [x2 x3 -- x4] -- x )
Z\ e.g. 0 5 6 7 ' + reduce  =  5 6 7 + +  =  18 )
Z >r dup 0= if
Z  ." no operand" abort
Z endif
Z begin
Z  over 0= not while
Z  r@ execute
Z repeat \ 0 x )
Z swap drop rdrop ;
Z
Z7 constant concatenation-id
Z: (- \ -- n 0 )
Z concatenation-id 0 ;
Z: -) \ n 0 syntax-expr1 syntax-expr2 .. -- syntax-expr )
Z ['] concat reduce
Z swap concatenation-id ?pairs ;
Z
Z8 constant alternative-id
Z: (| \ -- n 0 )
Z alternative-id 0 ;
Z: |) \ n 0 syntax-expr1 syntax-expr2 .. -- syntax-expr )
Z ['] alt reduce
Z swap alternative-id ?pairs ;
Z
Z: (( (| (- ;
Z: )) -) |) ;
Z: || -) (- ;
STUNKYFLUFF
#
#
echo Extracting graylist.f83
sed 's/^Z//' >graylist.f83 <<\STUNKYFLUFF
Z( $Id: graylist.f83,v 1.1 90/04/18 14:20:28 ertl Exp Locker: anton $ )
Z( Copyright 1990 Martin Anton Ertl )
Z( This program is distributed WITHOUT ANY WARRANTY. )
Z( See gray.doc or gray.f83 for the license. )
Z( list construct for parsing )
Z( a b && is the same as < a < b a > * > )
Z
Z( simple solution )
Z( : && \ syntax-expr1 syntax-expr2 -- syntax-expr3 )
Z( over concat ** concat ; )
Z
Zbinary-syntax-expr
Z cell context-var test-set
Zconstant list-syntax-expr
Z
Z: compute-list ( -- first follow )
Z operand1 compute dup if
Z  swap operand2 get-first union swap
Z endif ;
Z
Z: propagate-list ( follow -- )
Z operand2 compute if
Z  operand1 get-first union
Z endif
Z union
Z dup operand1 propagate ( follow1 )
Z operand1 compute if
Z  union
Z else
Z  swap drop
Z endif
Z operand2 propagate ;
Z
Z: generate-list ( -- )
Z [compile] begin
Z operand1 generate
Z test-set @ compile-test
Z [compile] while
Z operand2 generate
Z [compile] repeat ;
Z
Z: pass2-list ( -- )
Z operand2 compute if
Z  operand1 get-first union
Z endif
Z dup test-set !
Z follow-set @ check-conflict
Z pass2-binary ;
Z
Zcreate list-map
Z', compute-list
Z', propagate-list
Z', generate-list
Z', pass2-list
Z
Z: && ( syntax-expr1 syntax-expr2 -- syntax-expr3 )
Z list-map make-binary 0 , ;
Z
STUNKYFLUFF
#
#
echo Extracting mini.f83
sed 's/^Z//' >mini.f83 <<\STUNKYFLUFF
Z( $Id: mini.f83,v 1.1 90/04/18 14:21:23 ertl Exp Locker: anton $ )
Z( Copyright 1990 Martin Anton Ertl )
Z( This program is distributed WITHOUT ANY WARRANTY. )
Z( See gray.doc or gray.f83 for the license. )
Z( a small compiler )
Z( to compile a program type "mini" and then type in the program )
Z( This creates a new word, that you must call to execute the program )
Z( you have to type one symbol and one character after the end of the )
Z( mini program because of the lookahead of parser and scanner )
Z( you can then call the program by its name )
Z( mini programs take their input from the stack and write their )
Z( output with . )
Z#include gray.f83
Z#include graylist.f83
Z
Z.( Loading mini ... ) cr
Z
Z( scanner )
Z( it is implemented using gray to give an example )
Z( that's probably not the best way )
Z255 max-member ( the whole character set )
Z
Zvariable tokenval 0 tokenval !
Z: token ( -- ) ( use: token name ) ( name: -- n )
Z ( defines a token that returns a unique value )
Z tokenval @ constant
Z 1 tokenval +! ;
Z
Ztoken ";"
Ztoken ","
Ztoken ":="
Ztoken "="
Ztoken "#"
Ztoken ">"
Ztoken "+"
Ztoken "-"
Ztoken "*"
Ztoken "("
Ztoken ")"
Ztoken Ident
Ztoken Number
Z
Zvocabulary keywords keywords definitions
Ztoken PROGRAM
Ztoken VAR
Ztoken BEGIN
Ztoken END
Ztoken Read
Ztoken Write
Ztoken IF
Ztoken THEN
Ztoken WHILE
Ztoken DO
Zforth definitions
Z
Zvariable numval
Zvariable identp
Zvariable identlen
Z
Z: adds ( addr1 c -- addr1+1 )
Z ( accumulates char to string )
Z over c! 1+ ;
Z
Z: key/ident ( addr -- n )
Z ( checks string at addr for keyword and returns token value )
Z ['] keywords lookup if
Z  execute
Z else
Z  drop Ident
Z endif ;
Z
Zvariable ch
Z
Z: testchar? ( set -- f )
Z ch c@ member? ;
Z' testchar? test-vector !
Z
Z: ?nextchar ( f -- )
Z not abort" non-mini character or '=' missing after ':'"
Z key ch c! ;
Z
Z: .. ( c1 c2 -- set )
Z ( creates a set that includes the characters c, c1<=c<=c2 )
Z empty copy-set
Z swap 1+ rot do
Z  i over add-member
Z loop ;
Z
Z: ` ( -- terminal ) ( use: ` c )
Z ( creates anonymous terminal for the character c )
Z [compile] ascii singleton ['] ?nextchar make-terminal ;
Z
Zascii a ascii z ..  ascii A ascii Z ..  union  ' ?nextchar  terminal letter
Zascii 0 ascii 9 ..  ' ?nextchar  terminal digit
Z0 32 ..  ' ?nextchar  terminal space
Z
Z(( space **
Z   (( ` ;      {{ ";"  }}
Z   || ` ,      {{ ","  }}
Z   || ` : ` =  {{ ":=" }}
Z   || ` =      {{ "="  }}
Z   || ` #      {{ "#"  }}
Z   || ` >      {{ ">"  }}
Z   || ` +      {{ "+"  }}
Z   || ` -      {{ "-"  }}
Z   || ` *      {{ "*"  }}
Z   || ` (      {{ "("  }}
Z   || ` )      {{ ")"  }}
Z   || {{ 0 }}
Z      (( {{ 10 * ch c@ + ascii 0 - }} digit )) ++
Z      {{ numval !  Number }}
Z   || {{ here identp ! here ch c@ adds }} letter
Z      (( {{ ch c@ adds }} (( letter || digit )) )) **
Z      {{ 0 adds  here - identlen !  here key/ident }}
Z   ))
Z)) <- symbol
Z
Zsymbol parser scan
Z
Z
Z( parser )
Ztokenval @ 1- max-member
Z
Zvocabulary variables ( for mini variables )
Z
Zvariable sym
Z
Z: testsym? ( set -- f )
Z sym @ member? ;
Z' testsym? test-vector !
Z
Z: ?nextsym ( f -- )
Z not abort" syntax error"
Z scan sym ! ;
Z
Z: t ( n -- ) ( use: token t name )
Z singleton ['] ?nextsym terminal ;
Z
Z";" t ";"
Z"," t ","
Z":=" t ":="
Z"=" t "="
Z"#" t "#"
Z">" t ">"
Z"+" t "+"
Z"-" t "-"
Z"*" t "*"
Z"(" t "("
Z")" t ")"
ZIdent t Ident
ZNumber t number
ZPROGRAM t PROGRAM
ZVAR t VAR
ZBEGIN t BEGIN
ZEND t END
ZRead t "Read"
ZWrite t "Write"
ZIF t IF
ZTHEN t THEN
ZWHILE t WHILE
ZDO t DO
Z
Z: $prog ( addr -- )
Z ( defines colon-def with the whose name is pointed to by addr )
Z >r here 0 1 r> entry ] ;
Z
Z: $var ( addr -- )
Z ( defines variable with the name of the 0-terminated string at addr )
Z ( very tile-dependent )
Z ['] variables lookup abort" variable already defined"
Z >r 0 0 2 r> entry ;
Z
Z: getvar ( addr -- word )
Z ( get the execution address of the var whose name is pointed to by addr )
Z ['] variables lookup not abort" variable undefined" ;
Z
Z: <> ( n1 n2 -- f )
Z = not ;
Z
Znonterminal Stat
Znonterminal Expr
Z
Z(( {{ numval @ }} number )) <- Number
Z
Z\ (( {{ identp @ }} ident )) <- Ident
Z
Z(( Number {{ [compile] literal }}
Z|| {{ identp @ getvar compile, compile @ }} Ident
Z|| "(" Expr ")"
Z)) <- Factor
Z
Z(( Factor (( "*" Factor {{ compile * }} )) ** )) <- Term
Z
Z(( Term  (( (( "+" {{ ['] + }} || "-" {{ ['] - }} )) Term {{ compile, }} )) **
Z)) Expr rule
Z
Z(( Expr
Z   (( "=" {{ ['] = }} || "#" {{ ['] <> }} || ">" {{ ['] > }} ))
Z   Expr {{ compile, }}
Z)) <- Cond
Z
ZStat ";" && ?? <- StatSeq
Z
Z(( "Read" {{ identp @ getvar compile, compile ! }} Ident )) <- ReadStat
Z
Z(( "Write" Expr {{ compile . }} )) <- WriteStat
Z
Z(( {{ identp @ getvar }} Ident ":=" Expr {{ compile, compile ! }}
Z)) <- AssStat
Z
Z(( IF Cond {{ [compile] if }} THEN StatSeq END {{ [compile] endif }}
Z)) <- IfStat
Z
Z(( {{ [compile] begin }} WHILE Cond {{ [compile] while }} DO
Z   StatSeq END {{ [compile] repeat }}
Z)) <- WhileStat
Z   
Z(( ReadStat || WriteStat || AssStat || IfStat || WhileStat )) Stat rule
Z
Z(( VAR {{ variables definitions }}
Z   (( {{ identp @ $var }} Ident )) "," &&
Z   {{ forth definitions }}
Z)) <- Decl
Z
Z(( PROGRAM {{ identp @ identlen @ align allot }} Ident  Decl ??
Z   {{ $prog }} BEGIN StatSeq {{ [compile] ; }} END 
Z)) <- Program
Z
ZProgram parser parsemini
Z
Z: mini ( -- ) ( use: mini name )
Z true ?nextchar true ?nextsym parsemini ;
STUNKYFLUFF
#
#
echo Extracting oberon.f83
sed 's/^Z//' >oberon.f83 <<\STUNKYFLUFF
Z\ $Id: oberon.f83,v 1.1 90/04/18 14:21:47 ertl Exp Locker: anton $ )
Z( Copyright 1990 Martin Anton Ertl )
Z( This program is distributed WITHOUT ANY WARRANTY. )
Z( See gray.doc or gray.f83 for the license. )
Z\ parser for oberon )
Z\ i chose oberon, because it has a moderately complex grammar, )
Z\ not for its qualities as a language )
Z\ this is just a parser, without any semantic actions )
Z\ it was not tested )
Z\ the grammar was taken from: )
Z\ N.Wirth, The Programming Language Oberon, )
Z\ Software - Practice and Experience, 18, 671-690 (July 1988)
Z\ corrections appeared in the january 89 issue, i believe )
Z
Z\ space requirements on a 16-bit fig-forth using graylist.f83 )
Z\ grammar:         8104 bytes )
Z\ generated code:  3551 bytes )
Z\ generated total: 5719 bytes )
Z\ context-stack:    220 bytes )
Z\ return-stack:     720 bytes   WARNING: you must enlarge TILE's return-stack )
Z\ the data-stack is not critical- mine can only hold 60 cells )
Z\ if your return-stack cannot hold much, change the does> part )
Z\ of method: pop 3 cells off the return stack and save them )
Z\ elsewhere until after the execute )
Z\ generating the parser takes a while: 24.5 seconds on my 4Mhz 6502 system )
Z
Z\ the grammar contains four conflicts, which are all harmful, )
Z\ i.e. the generated parser will not parse all oberon programs )
Z\ in the qualident rule there is a confict between the two idents )
Z\ designator doesn't know, whether a "(" means a type guard or a procedure call)
Z\ Procedure- and ForwardDeclaration have a conflict in a DeclarationSequence )
Z\ in statement there's a classical conflict between assigment and ProcedureCall)
Z
Z63 max-member
Z
Zvariable tcount 0 tcount !
Z: t \ -- )
Z tcount @ singleton ['] abort terminal
Z 1 tcount +! ;
Z
Zt integer
Zt real
Zt CharConstant
Zt string
Zt ident
Zt "+"
Zt "-"
Zt "*"
Zt "/"
Zt "~"
Zt "&"
Zt "."
Zt ","
Zt ";"
Zt "|"
Zt "("
Zt ")"
Zt "["
Zt "]"
Zt ":="
Zt "^"
Zt "="
Zt "#"
Zt "<"
Zt ">"
Zt "<="
Zt ">="
Zt ":"
Zt ".."
Zt "{"
Zt "}"
Z
Zt ARRAY        t IN        t THEN
Zt BEGIN        t IS        t TO
Zt CASE         t LOOP      t TYPE
Zt CONST        t MOD       t UNTIL
Zt DEFINITION   t MODULE    t VAR
Zt DIV          t NIL       t WHILE
Zt DO           t OF        t WITH
Zt ELSE         t OR
Zt ELSIF        t POINTER
Zt END          t PROCEDURE
Zt EXIT         t RECORD
Zt IF           t REPEAT
Zt IMPORT       t RETURN
Z
Z: && \ syntax-expr1 syntax-expr2 -- syntax-expr3 )
Z over concat ** concat ; 
Z
Znonterminal factor
Znonterminal expression
Znonterminal type
Znonterminal statement
Znonterminal DeclarationSequence
Z
Z(( integer || real )) <- number
Z
Z(( (( ident "." )) ?? ident )) <- qualident
Z
Zexpression <- ConstExpression
Z(( ident "=" ConstExpression )) <- ConstantDeclaration
Z
ZConstExpression <- length
Z(( ARRAY length "," && OF type )) <- ArrayType
Z
Zident "," && <- IdentList
Z(( IdentList ":" type )) ?? <- FieldList
ZFieldList ";" && <- FieldListSequence
Zqualident <- BaseType
Z(( RECORD (( "(" BaseType ")" )) ?? FieldListSequence END )) <- RecordType
Z
Z(( POINTER TO type )) <- PointerType
Z
Z(( (( ARRAY OF )) ** qualident )) <- FormalType
Z(( "(" (( VAR ?? FormalType )) "," && ?? ")" (( ":" qualident )) ?? ))
Z <- FormalTypeList
Z(( PROCEDURE FormalTypeList ?? )) <- ProcedureType
Z
Z(( qualident || ArrayType || RecordType || PointerType || ProcedureType ))
Z type rule
Z(( ident "=" type )) <- TypeDeclaration
Z
Z(( IdentList ":" type )) <- VariableDeclaration
Z
Zexpression "," && <- ExpList
Z(( qualident (( "." ident || "[" ExpList "]" || "(" qualident ")" || "^" )) **
Z)) <- designator
Z
Z(( "(" ExpList ?? ")" )) <- ActualParameters
Z(( expression (( ".." expression )) ?? )) <- element
Z(( "{" element "," && ?? "}" )) <- set
Z(( number || CharConstant || string || NIL || set ||
Z   designator ActualParameters ?? || "(" expression ")" || "~" factor ))
Z factor rule
Z(( "*" || "/" || DIV || MOD || "&" )) <- MulOperator
Zfactor MulOperator && <- term
Z(( "+" || "-" || OR )) <- AddOperator
Z(( (( "+" || "-" )) ?? term AddOperator && )) <- SimpleExpression
Z(( "=" || "#" || "<" || "<=" || ">" || ">=" || IN || IS )) <- relation
Z(( SimpleExpression (( relation SimpleExpression )) ?? )) expression rule
Z
Z(( designator ":=" expression )) <- assignment
Z
Z(( designator ActualParameters ?? )) <- ProcedureCall
Z
Zstatement ";" && <- StatementSequence
Z
Z(( IF expression THEN StatementSequence
Z   (( ELSIF expression THEN StatementSequence )) **
Z   (( ELSE StatementSequence )) ??
Z   END
Z)) <- IfStatement
Z
Z(( ConstExpression (( ".." ConstExpression )) ?? )) <- CaseLabels
ZCaseLabels "," && <- CaseLabelList
Z(( CaseLabelList ":" StatementSequence )) ?? <- case
Z(( CASE expression OF case "|" && (( ELSE StatementSequence )) ?? END ))
Z<- CaseStatement
Z
Z(( WHILE expression DO StatementSequence END )) <- WhileStatement
Z
Z(( REPEAT StatementSequence UNTIL expression )) <- RepeatStatement
Z
Z(( LOOP StatementSequence END )) <- LoopStatement
Z
Z(( WITH qualident ":" qualident DO StatementSequence END )) <- WithStatement
Z
Z(( assignment || ProcedureCall ||
Z   IfStatement || CaseStatement || WhileStatement || RepeatStatement ||
Z   LoopStatement || WithStatement || EXIT || RETURN expression ??
Z)) ?? statement rule
Z
Z(( VAR ?? IdentList ":" FormalType )) <- FPSection
Z(( "(" FPSection ";" && ?? ")" (( ":" qualident )) ?? )) <- FormalParameters
Z
Z(( DeclarationSequence (( BEGIN StatementSequence )) ?? END )) <- ProcedureBody
Z(( PROCEDURE "*" ?? ident FormalParameters ?? )) <- ProcedureHeading
Z(( ProcedureHeading ";" ProcedureBody ident )) <- ProcedureDeclaration
Z(( PROCEDURE "^" ident FormalParameters ?? )) <- ForwardDeclaration
Z(( (( CONST (( ConstantDeclaration ";" )) ** )) ??
Z   (( TYPE (( TypeDeclaration ";" )) ** )) ??
Z   (( VAR (( VariableDeclaration ";" )) ** )) ??
Z   (( ProcedureDeclaration ";" || ForwardDeclaration ";" )) **
Z)) DeclarationSequence rule
Z
Z(( (( CONST (( ConstantDeclaration ";" )) ** )) ??
Z   (( TYPE (( TypeDeclaration ";" )) ** )) ??
Z   (( VAR (( VariableDeclaration ";" )) ** )) ??
Z   (( ProcedureHeading ";" )) **
Z)) <- DefSequence
Z(( ident (( ":" ident )) ?? )) <- import
Z(( IMPORT import "," && ";" )) <- ImportList
Z(( MODULE ident ";" ImportList ?? DeclarationSequence
Z    (( BEGIN StatementSequence )) ?? END ident "." )) <- module
Z(( DEFINITION ident ";" ImportList ?? DefSequence END ident "." )) <- definition
Z(( module || definition )) <- CompilationUnit
Z
ZCompilationUnit parser oberon
Z
STUNKYFLUFF
#
#
echo Extracting test.f83
sed 's/^Z//' >test.f83 <<\STUNKYFLUFF
Z( $Id: test.f83,v 1.1 90/04/18 14:21:56 ertl Exp Locker: anton $ )
Z#include gray.f83
Z: xxx ;
Z.( else )
Z#include else.f83
Zcr
Zforget xxx
Z.( oberon )
Z: xxx ;
Z#include oberon.f83
Zcr
Zforget xxx
Z: xxx ;
Z\ #include graylist.f83
Z\ #include oberon.f83 
Zforget xxx
Z: xxx ;
Z.( calc ) cr
Z#include calc.f83
Z? 2*3-5/4=
Zcr
Zforget xxx
Z: xxx ;
Z.( mini ) cr
Z#include mini.f83
Zmini
ZPROGRAM test
ZVAR laufVar, i, j, SUM
ZBEGIN
ZRead i;
ZlaufVar := i+1;
Zj := 0 - 1;
ZSUM := j;
ZWHILE laufVar > 0 DO
Z	j := j + 2;
Z	IF j#3 THEN SUM := SUM+j END;
Z	laufVar := laufVar -1
Z	END;
ZIF i+1 > 0 THEN j:=j-2 END;
ZIF i > 0 THEN SUM := 3+SUM END;
ZWrite SUM - (j - 2 * 2 + 11 + 9 - (3*5)) 
ZEND;
Z5 test
Zcr
Zforget xxx
STUNKYFLUFF
#
#
echo Extracting test.mini
sed 's/^Z//' >test.mini <<\STUNKYFLUFF
ZPROGRAM test
ZVAR laufVar, i, j, SUM
ZBEGIN
ZRead i;
ZlaufVar := i+1;
Zj := 0 - 1;
ZSUM := j;
ZWHILE laufVar > 0 DO
Z	j := j + 2;
Z	IF j#3 THEN SUM := SUM+j END;
Z	laufVar := laufVar -1
Z	END;
ZIF i+1 > 0 THEN j:=j-2 END;
ZIF i > 0 THEN SUM := 3+SUM END;
ZWrite SUM - (j - 2 * 2 + 11 + 9 - (3*5)) 
ZEND
Z 
Z
Z
STUNKYFLUFF
#
#
echo Extracting test.out
sed 's/^Z//' >test.out <<\STUNKYFLUFF
ZTILE Forth version 3.26, Copyright (c) 1990, by Mikael Patel
ZLoading Internal definitions...
ZLoading Enumeration definitions...
ZLoading Bit Field definitions...
ZLoading Structure definitions...
ZLoading Block definitions...
ZLoading Lists definitions...
ZLoading Sets definitions...
ZLoading gray ... Copyright 1990 Martin Anton Ertl; NO WARRANTY 
Zelse 
Zconflict:2 
Zoberon 
Zconflict:4 
Zconflict:15 
Zconflict:57 
Zconflict:4 
Zcalc 
Z5 
Zmini 
ZLoading mini ... 
Z25 
STUNKYFLUFF
echo ALL DONE BUNKY!
exit 0




