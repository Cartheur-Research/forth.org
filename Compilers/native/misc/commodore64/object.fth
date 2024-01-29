
type object bfth

 
file: object.fth
 
scr #0
 0> // object.fth
 1>
 2>
 3> created:    8/17/86
 4> modified:   8/22/86
 5> backed up:  8/22/86
 6>
 7>
 8>
 9> from types.doc, byte august 1986
10> written by: dick pountain
11>
12>
13>
14>
15>
 
 
 
file: object.fth
 
scr #1
 0> // object.fth load block
 1> free . .( bytes in dictionary) cr
 2> 2 fh 14 fh thru  // load type extensions to forth
 3> cr free . .( bytes in dictionary)
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
 
 
 
file: object.fth
 
scr #2
 0>
 1>
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
 
 
 
file: object.fth
 
scr #3
 0> // type definitions
 1>
 2> // working variables for object compiler
 3> variable size  // holds size of type
 4> variable inherit  // holds address of inherited type
 5> variable ops      // holds address of end of ops vocab
 6> variable stash   // temporary store for current vocab
 7> variable public  // holds link to ordinary vocab
 8> variable lastlocal // holds address of last word in type
 9> variable in.type.def? // flag, are we in type def?
10> variable val    // flag, has a val index been declared
11>
12>
13>
14>
15>
 
 
 
file: object.fth
 
scr #4
 0>    // type definitions
 1>
 2>  // make a third stack to hold current objects address
 3>  // size determines how deeply definitions can be nested
 4> create ostack   here 16 + ,  16 allot
 5>
 6> // push parrameter stack to object stack
 7> : opush   ostack -2 over +!   @ ! ; // <n--->
 8>
 9> // pop object stack and discard
10> : opop    2 ostack +! ; // <--->
11>
12> // copy top of ostack and add to parameter stack
13> : ocop+   ostack @ @   + ;  // <n---m>
14>
15>
 
 
 
file: object.fth
 
scr #5
 0>    // type definitions
 1>
 2> // compile offset into instance variable and bump total
 3> : offset   size @ 2+ ,  size +! ; // <size--->
 4>
 5> // purely for brevity
 6> : complit  [compile] literal ;
 7>
 8> // compile code to add offset into object body
 9> : compile.addoff   complit compile ocop+ ;
10>
11> // create a new instance variable of size bytes
12> : var       create  offset     // <size--->
13>                     immediate
14>             does>   @  compile.addoff ;
15>
 
 
 
file: object.fth
 
scr #6
 0>    // type definitions
 1> // open a type declaration
 2> : type>   latest public !      // nfa of last public word
 3>           create         // make header
 4>           latest lastlocal ! // store its pfa
 5>           0 size !   // init
 6>           in.type.def? on
 7>           inherit off ;
 8>
 9>
10> // mark boundary which hides instance variables
11> : ops>  latest      // address following last var
12>         0 c,      // make dummy name field
13>         latest , // link field points to last var
14>         // dup context @ ! let forth know about it
15>         n>link  ops ! ;  // save its lfa
 
 
 
file: object.fth
 
scr #7
 0>    // type defintions
 1>
 2> // save current vocab.  set ops vocab
 3> : unlock     context @ dup  @ stash ! !  ;  // <key--->
 4>
 5> // restore current vocab
 6> : lock         stash @ context @ ! ;
 7>
 8> // look up an operation in its type vocab  <key---cfa>
 9> : findop    bl word swap        // get opertion name
10>             unlock find lock    // find it
11>             0= abort" not a valid operator for this type" ;
12>
13>
14>
15>
 
 
 
file: object.fth
 
scr #8
 0>    // type definitions
 1>
 2> // execute an operation immeditally, if found
 3> : do.op   swap opush findop execute opop ;  // <addr,key--->
 4>
 5> // compile operation calling sequence <cfa--->
 6> : compile.call  compile opush , compile opop ;
 7>
 8> // look up operation and compile it
 9> : compile.op   findop swap complit  // <addr,key--->
10>               compile.call ;
11>
12> // fetch size field contents from instanc variable or type
13> : sz@  2+ @ ;  // <addr---size>
14>
15> : self  ;  // for readability only
 
 
 
file: object.fth
 
scr #9
 0>    // type definitions
 1>
 2> // create an instance variable of prdefined type <addr--->
 3> : make.structvar   dup sz@  // get size
 4>                    swap @   // get key
 5>                    create , offset // store key and size
 6>                    immediate
 7>              does> dup @ swap sz@ 2- // get key and offset
 8>                    compile.addoff // compile code
 9>                    findop  // to treat as
10>                    compile.call ; // an object
11>
12> // compile or intrepret an op occording to state
13> : do.or.comp    state @ if compile.op  // <addr,key--->
14>                       else do.op then ;
15>
 
 
 
file: object.fth
 
scr #10
 0>    // type definitions
 1>
 2> // allot space inited to 0
 3> : allotz   dup here swap 0 fill allot ;  // <n--->
 4>
 5> // execute an operation called init if there is one
 6> : initialize      swap opush
 7>      unlock   " init" find lock
 8>      if execute else drop then opop ;
 9>
10> // create a new instance of a type <addr--->
11> : make.instance        create here swap
12>                   dup @ dup , // store key in instance
13>                   swap sz@ allotz  // allot its storage
14>                   initialize  immediate
15>                   does>  dup @ do.or.comp ;
 
 
 
file: object.fth
 
scr #11
 0>    // type definitions
 1>
 2> : includes>   ' >body @ inherit ! ; // inherit old type
 3>
 4> // juggle dictionary pointers to seal the type body
 5> : links   latest n>link public @ swap ! // <--->
 6>           lastlocal  @ ops @ !
 7>           inherit @ lastlocal @ body> >link ! ;
 8>
 9> // close type definition
10> : endtype>   latest create links  // close body
11>              , size @ ,  // store key and size
12>              in.type.def? off
13>              does>  in.type.def? @ if make.structvar
14>                     else make.instance then ;
15>
 
 
 
file: object.fth
 
scr #12
 0>    // array definitions
 1>
 2> // calculates address of array element
 3> : index+  rot * + ;  // <index,pfa,width---address>
 4>
 5> // interpret an array operation <index,pfa,key--->
 6> : array.do.op       findop  // get operation cfa
 7>                      rot rot 4 + dup @  // get width of element
 8>                      index+ // calculate element address
 9>                      opush execute opop ; // do it
10>
11> // place index on stack at compile time
12> : val[    val on [compile] [ ; immediate
13>
14> // reset the val flag
15> : val.off   val off ;
 
 
 
file: object.fth
 
scr #13
 0>    // array definitions
 1>
 2> // compile an array oper <(index),pfa,key--->
 3> : array.comp.op     findop >r   // get op cfa and stash it
 4>                      4 + dup @   // get width of array
 5>                      val @       // index at compile time
 6>                      if  index+ complit // compile element addr
 7>                      else swap complit complit // or code to cal
 8>                      compile index+
 9>                      then
10>                      r> compile.call val.off ; // compile op cal
11>
12> // compile op interprt an array op <index,pfa,key--->
13> : array.do.or.comp     state @ if  array.comp.op
14>                        else  array.do.op
15>                   then ;
 
 
 
file: object.fth
 
scr #14
 0>    // array definitions
 1> // create a typed array as an instance variable <count,pfa--->
 2> : array.var     create dup @ , over , // store key and count
 3>       sz@  dup ,  // store width of element
 4>       *           // size = count*width
 5>       offset      // store offset
 6>       immediate
 7>       does>  dup @ // get key
 8>       findop  >r   // get op cfa and stash it
 9>       dup 6 + @    // get offset
10>       2- swap 4 + @ // get width
11>       val @ if index+ compile.addoff // compile element add
12>           else swap complit complit // or code to
13>           compile index+  // calc it at runtime
14>           compile ocop+
15>       then r> compile.call val.off ;
 
 
 
file: object.fth
 
scr #15
 0>    // array definitions
 1>
 2> // make a new array instance <count,pfa--->
 3> : make.array   create 2dup @ , , // store key anad count
 4>                sz@  dup , swap   // store width
 5>                * allotz  // allot the space
 6>                immediate
 7>                does> dup @ array.do.or.comp ;
 8>
 9> // create an array object or variable <count--->
10> : array-of     ' >body
11>                in.type.def? @
12>                if  array.var
13>                else  make.array
14>                then ;
15>
 
 
 
file: object.fth
 
scr #16
 0>    // internal data structures
 1> object     <---------width-------->
 2> +----------+-----+----------------+
 3> !  header  ! key ! storage fields !
 4> +----------+-----+----------------+
 5>
 6> array-of objects
 7> +----------+-----+-------+-------+-----------+
 8> !  header  ! key ! count ! width ! elements  !
 9> +----------+-----+-------+-------+-----------+
10>
11>
12> type defining word
13> +----------+-----+------+
14> !  header  ! key ! size !
15> +----------+-----+------+
 
 
 
file: object.fth
 
scr #17
 0>    // internal data structures
 1> var name
 2> +----------+--------+
 3> !  header  ! offset !
 4> +----------+--------+
 5>
 6> structure var name
 7> +----------+-----+--------+
 8> !  header  ! key ! offset !
 9> +----------+-----+--------+
10>
11> array var name
12> +----------+-----+-------+-------+-----------+
13> !  header  ! key ! count ! width ! offset    !
14> +----------+-----+-------+-------+-----------+
15>
 
 
 
 
R; T=0.15/0.81 17:52:59


