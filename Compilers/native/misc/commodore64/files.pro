
A proposal for a file standard:

This proposal is for a standard set of file words to be used by forth.
It would be handled as a standard extension to Forth 83 and should allow
one to move code using files from one system to another.  It is not meant
to be the last word in files, many features could be added and this only
addresses open, close, read and write.  Random access isn't addressed.
The code supplied here is for bforth, a Public Domain forth 83 for C64.
In most cases it couldn't be transportable since each os impliments
files differently.  What is important is that words using these "higher"
level file words would be transportable from VAX, to IBM PC, to C64 or
whatever.


Transportable words:

    file  : compiling word, Compiles word following it as file pointer.
                           ex.  file stdin  // makes a file named stdin
            It also reserves space for the file buffers and any other
            system dependent information for a file.

            Runtime:  <---a> returns address of file area.  Any
            use of this file area other than passing around it's address
            will be system dependent.

wrmode : Returns pointer to "something" which will indicate that
         the file is to be opened for write
rdmode : Same as write mode but file is opened for reading.

fclose : <a---> a is a file pointer.  Closes file pointed to by a.

sterror : <a---> a is the address of a string with count stored as
                 first byte.  Type out string at a and then does
                 system dependent shutdown.  Normally closes all
                 open files
fopen  : <a1,a2---a3> a1 is the address of the file name, length
                 stored as first byte.  a2 is the pointer to the mode
                 the file is to be opened in.  a3 is a pointer to
                 the opened file.

ststartup : Starts up standard file package.  Opens stdin and stdout,
                 the standard input and standard output files.

stexit : Closes down standard file system.  Closes stdin, stdout, and
                all files opened with fopen.

Note on getc:  Normal conditions signaled by s = 0, if s is something
               else then an abnormal end has occured (EOF, disk error,
               what ever).

getc : <a---c,s> Takes a, a file pointer, returns s, status, and c, byte
                 from file.

putc : <a,c---> Takes c, the byte to write, and a, the file pointer to
                 write it to and writes the file.
getchar : <---c,s> Getc from stdin

putchar : <c---> putc to stdout.


scr #3
 0> // software tools load screen
 1>
 2> 1 fh 7 fh thru
 3>
 4>
 5>
 6>
 7>
 8>
 9>
10>
11>
12>
13>
14>
15>

Screen 4 is nontransportable

file: softwtool.fth

scr #4
 0>    // software tools: rel-lfn 11/12/85
 1> variable maxlfn 1 maxlfn !  // holds last used lfn
 2>
 3> : clear-lfn-table
 4>    1 maxlfn ! ;
 5>
 6> : close-lfn ( close all open lfn )
 7>    maxlfn @ 0 do
 8>        i close loop clear-lfn-table ;
 9>
10> : rel-lfn ( release a lfn entry n--- )
11>        maxlfn @ 1- = if -1 maxlfn +! then ;
12>
13>
14>
15>

Screen 5 is nontransportable

file: softwtool.fth

scr #5
 0>    // software tools: getlfn 11/12/85
 1> : getlfn ( returns a lfn or aborts )
 2>       maxlfn @
 3>       dup 128 > abort" not enough lfn"
 4>       1 maxlfn +! ;
 5> : wrmode " ,w" ; : rdmode " ,r" ;
 6> : kbd$ " kbd:,r" ; : scr$ " scr:,w" ;
 7>
 8> : file  // <text> compiling
 9>         // <---a> runtime.  Returns file pointer
10>     create 0 , does> ;
11>
12>
13>
14>
15>



file: softwtool.fth

scr #6
 0>    // software tools: fclose sterror ?derr
 1> : fclose ( <n---> closes file pointed to by n )
 2>       @ dup rel-lfn close ;
 3>
 4> : sterror ( <n---> n is a string typed out )
 5>           ( trace-back then abort )
 6>       cmdoff ( reset i/o )
 7>       cr $? ( type out error message )
 8>       unravel
 9>       cr 15 close
10>       true abort" error exit" ;
11>
12> 13 constant <ret> // return ascii code
13>
14>
15>



file: softwtool.fth

scr #7
 0>    // software tools: ?d 12/08/85
 1> : ?derr ( read open disk error channel )
 2> ( file 15 must be open as disk error channel )
 3>     15 (cmdin) ioerr pad 1+ begin
 4>        key dup <ret> = not while
 5>           over c! 1+ repeat
 6>     drop pad - pad c!
 7>     ( check to see if error )
 8>     pad 1+ c@ ascii 0 = not if
 9>         pad sterror
10>     then ;
11>
12> : ?d ( types out disk error message )
13>     15 close
14>     15 8 15 0 0 (open) ioerr
15>     ?derr pad $? 15 close ;



file: softwtool.fth

scr #8
 0>    // software tools: devnum fopen 11/21/85
 1> : devnum ( <n---m> n is string address
 2>            m is device number )
 3>      dup kbd$ $= if $blank 0 else
 4>      dup scr$ $= if $blank 3 else
 5>                        drop 8
 6>            then then ;
 7> variable temp-filedesc
 8> : fopen ( <n1,n2---m> n1 is file name address
 9>           n2 is mode string address
10>           m is lfn )
11>      over swap
12>      $+ >r getlfn dup
13>      r@ devnum
14>      over 1+ r> count (open) ioerr ?derr
15>      temp-filedesc c! 0 temp-filedesc 1+ c! temp-filedesc ;



file: softwtool.fth

scr #9
 0>    // software tools: startup 11/10/85
 1> ( startup reads the stack and opens )
 2> ( then nessary files )
 3> variable stdin
 4> variable stdout
 5> 40 string stfntemp
 6> : ststartup ( standard sftware tools start )
 7>    0 stdin ! 0 stdout ! cr 15 close
 8>    15 8 15 " i0:" count (open) ioerr
 9>    bl text
10>    pad $length if ( input file )
11>       pad rdmode fopen ( open it ) @ stdin ! then
12>    bl text pad $length if ( output file )
13>       pad wrmode
14>       fopen ( open it ) @ stdout ! then ;
15>



file: softwtool.fth

scr #10
 0>    // software tools: stexit getchar putchar 11/11/85
 1> : stexit ( exit and close files )
 2>      stdin @ close stdout @ close
 3>      cmdoff close-lfn
 4>      15 close ;
 5> : getc ( get a character from file pointer )
 6>      >r r@ 1+ c@ 0= if r@ 1+ c@ r@ c@ get# status
 7>         r@ 1+ c! swap else
 8>         0 r@ 1+ c@ then r> drop ;
 9> : putc ( put a character to file pointer )
10>      c@ put# status ;
11> : getchar ( get from stdin )
12>      stdin getc ;
13>
14> : putchar ( put to stdout )
15>      stdout putc ;



file: softwtool.fth

scr #11
 0> // copyfile software tool
 1> 1 fh load
 2>
 3>
 4>
 5>
 6>
 7>
 8>
 9>
10>
11>
12>
13>
14>
15>



file: softwtool.fth

scr #12
 0>    // copyfile software tool.  ascii only
 1> : stcopy   // copies stdin to stdout
 2>      begin
 3>         getchar 0= while
 4>         putchar drop
 5>      repeat drop ;
 6> : copyfile  // user command to copy stdin to stdout
 7>      ststartup stcopy stexit ;
 8> variable createlfn
 9> : createfile // <text> create file
10>      ststartup stdin @ (cmdin) ioerr
11>      begin
12>         key dup emit
13>         dup control "-" = not while
14>           putchar drop cmdoff repeat stexit ;
15>


