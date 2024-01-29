
This is a multitasking system for forth83.  It is specifically set up
to run on the c64 under blazin' forth.  It should be transportable
execpt for the interface into the system.  If you want this to run while
you are sitting there trying to think of the next command you need to
be able to get at EXPECT or KEY (if expect calls key) or, in my case
it was easier to rewrite quit and get the multitasker (RUN-TASKS is the
key word) installed there. From screen 92 on is the parts specific to
bforth.  STARTUP is a variable which holds the cfa of the word to run on
startup, warmstarts, and errors.
scr #86
 0> // multitasking system.
 1> 1 fh 5 fh thru
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

          blazin' forth '85  version rel64.1.0.2 -- ok to copy, not to sell.



scr #87
 0>    // multitasking table
 1> : table  // defining word
 2>          // on compile <n---> table <text> defines
 3>          // text as table of size n
 4>          // on run <n---a> n is index number, a is address
 5>    create dup c,
 6>    2* allot
 7>    does>
 8>    >r r@ c@ 1+ over
 9>    < abort" out of range"
10>    2* 1- r> + ;
11>
12>
13>
14>
15>


scr #88
 0>   // multitasking  variables and init
 1> variable curtask // current task number
 2> 20 constant maxtask  // max number of tasks
 3> maxtask table tasks  // define task table
 4> : main ;
 5> : init-tasks  // initilize the system
 6>     1 curtask !
 7>     maxtask 1 do
 8>       i tasks off loop
 9>     ['] main 1 tasks ! ;
10>
11>
12>
13>
14>
15>


scr #89
 0>    // multitask free-task? ?execute ?id.
 1> : free-task?  // <---n> returns n free or 0
 2>   0 maxtask 1 do
 3>      i tasks @ 0= if
 4>         drop i leave then
 5>      loop ;
 6>
 7> : ?execute // <a---> execute a if not 0
 8>      ?dup if execute then ;
 9>
10> : ?id.  // <a---> id. a if not 0
11>      ?dup if >name id. cr then ;
12>
13>
14>
15>

          blazin' forth '85  version rel64.1.0.2 -- ok to copy, not to sell.



scr #90
 0>    // multitask stopcurrent start-task new-task
 1> : stopcurrent  // <---> stop current task
 2>    curtask @ tasks off ;
 3> : start-task // <a---f> starts a, if f then good
 4>    free-task? dup if
 5>         tasks ! true
 6>       else
 7>         2drop false then ;
 8>
 9> : new-task  // <text> starts text as task
10>     ' start-task not abort" not started" ;
11>
12>
13>
14>
15>


scr #91
 0>    // multitask stop-task tasks? run-tasks
 1> : stop-task  // <text> stops task named text
 2>     ' maxtask 1 do
 3>           i tasks @ over = if
 4>              i tasks off then
 5>           loop drop ;
 6> : tasks?  // print out tasks running
 7>      cr
 8>        maxtask 1 do
 9>            i tasks @ ?id.
10>        loop ;
11>
12> : run-tasks  // runs the tasks
13>      maxtask 1 do
14>         i tasks @ ?execute
15>      loop ;


scr #92
 0> // new user interface
 1> 1 fh 2 fh thru  // new quit
 2> 3 fh load // install it
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

          blazin' forth '85  version rel64.1.0.2 -- ok to copy, not to sell.



scr #93
 0>    // new quit  single tasking
 1> defer sysprompt
 2> : <sysprompt>  // prompt
 3>    cr ." f83> " ;
 4> ' <sysprompt> is sysprompt
 5>
 6> : newquits // new version of quit single tasking
 7>     sp! blk off [compile] [
 8>     begin rp!
 9>     cmdoff
10>        state @ 0= if sysprompt then
11>        query run again ;
12>
13>
14>
15>


scr #94
 0>    // new quit multitasking
 1>
 2> : keywait // <---f> true if key waiting
 3>      198 c@ ;
 4> : newquitm // new version of quit multitasking
 5>     sp! blk off [compile] [
 6>     cmdoff
 7>     begin rp!
 8>        state @ 0= if sysprompt then
 9>        begin run-tasks keywait until
10>        cr query run again ;
11>
12>
13>
14>
15>


scr #95
 0>    // install new quits
 1>
 2>  ' newquits startup !
 3>
 4> : single  // single tasking
 5>      ['] newquits startup !
 6>      newquits ;
 7> : multi  // multi tasking
 8>      ['] newquitm startup !
 9>      init-tasks newquitm ;
10>
11> newquits
12>
13>
14>
15>

          blazin' forth '85  version rel64.1.0.2 -- ok to copy, not to sell.



scr #96
 0>    // doc for multitasking sys
 1> exit
 2> the multitaskin system has two different kinds of tasks
 3> forground, your typing task, and background, other tasks
 4> you have 1 forground and up to maxtask - 1 background
 5> tasks.  background tasks are individually executed so
 6> they need to return to let something else execute.
 7> they also shouldn't leave things on either the return or data
 8> stacks.  this is not the most fancy multitasking system
 9> in the world but it does work given the limitations
10> of forth on the c64 ( most impliment the
11> stack on page 0 and you'd have to move it for every
12> task to make things more general.
13> good luck with it and have fun.
14>   bruce.
15>


