\ This is an ANS Forth Program requiring
\ the Memory-Allocation word set

\ Copyright Leonard Zettel 1999
\ This material is released to the public domain without
\ warranty as to fitness for any purpose.
\ Use at your own risk.
 
\ Words to handle a user-created stack as a linked list with nodes of arbitrary size.

: stack CREATE 0 , ;

: node_size ( node-addr -- node-size) CELL+ @ ;

: n! ( n1 .. nn addr n --) \ Store n1 to nn in consecutive cells 
                           \ starting at addr.
  CELLS OVER + SWAP DO I ! 1 CELLS +LOOP ;

: n@ ( addr n -- n1 .. nn) \ Fetch n consecutive values starting at
                           \ addr + (wordsize)*(n-1) & leave them
                           \ on the stack.
  1- CELLS OVER + DO I @ -1 CELLS +LOOP ;

: node. ( addr --) \ Display the contents of the node at addr.
  DUP @ U. DUP CELL+ @ CELLS OVER + SWAP CELL+ DO I @ . 1 CELLS +LOOP ; 

: list. ( ptr --) \ Display the contents of the stack pointed to by ptr.
  CR DUP @ 0= IF ." stack empty" DROP EXIT THEN
  BEGIN @ ?DUP WHILE DUP node. CR REPEAT ; 
 
\ Thanks to Marcel Hendrix for noting that ALLOCATE works in address units. 
: push ( n1 .. nn addr --) \ Push n1 .. nn onto the stack pointed to 
                           \ by addr. nn is the node size in cells
  OVER >R R@ ( get_node) 
  CELLS ALLOCATE           \ Get node space
  ABORT" push: ALLOCATE failed." 
  >R DUP @                 \ Get address of node at the top of the node stack
  R@ ROT !                 \ Make new node top of stack
  R> R> n! ;               \ Store node contents.

: pop ( addr -- n1 .. nn) \ Pop stack pointed to by addr, leaving 
                          \ node values on the stack and freeing 
                          \ the node space.
  DUP @ DUP 0= ABORT" Empty user stack."
  DUP @ ROT ! DUP >R
  CELL+ DUP @ 1- n@ R> 
  FREE ABORT" pop: FREE failed" ;
  