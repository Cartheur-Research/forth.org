stack mystack 
mystack list.

567 3 mystack push 
mystack list.
0 3 567
 
mystack pop  

mystack push 
1009 885 234 5 mystack push 
mystack list.
 
\ ------------------------------------------------------
\ On SwiftForth(TM) the above gave the following 0utput:
\ stack empty
\ 0 3 567

\ 22282240 5 234 885 1009
\ 0 3 567

\ Note: your stack address (the 22282240 above) will almost certainly 
\ be different.