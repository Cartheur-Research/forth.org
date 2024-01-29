0 [IF] ==============================================================
     File Name: gforth-union.fs
       Version: 1.000
        Author: Timothy Trussell
          Date: 09/03/2011
   Description: UNION type construct
  Forth System: gforth-0.7.0
  Linux System: Ubuntu v10.04 i386, kernel 2.6.32-33
  C++ Compiler: gcc version 4.4.3 (Ubuntu 4.4.3-4ubuntu5)
_____________________________________________________________________

                          --- Abstract ---

Unions allow a section of memory to be accessed as different data
types, mapping each data type to the same physical address. Their
declarations and uses are similar to those of structures, but their
functionality is totally different

                         --- An Example ---

The Simple DirectMedia Layer (SDL) Event code utilizes a UNION type
construct in the definition of the SDL_Event structure, used to let
the system access anything that happens during program execution,
such as mouse moves, keypresses, joystick movement, and so on.

The definition of the SDL_Event UNION structure is:

    typedef union{
      Uint8 type;
      SDL_ActiveEvent active;
      SDL_KeyboardEvent key;
      SDL_MouseMotionEvent motion;
      SDL_MouseButtonEvent button;
      SDL_JoyAxisEvent jaxis;
      SDL_JoyBallEvent jball;
      SDL_JoyHatEvent jhat;
      SDL_JoyButtonEvent jbutton;
      SDL_ResizeEvent resize;
      SDL_ExposeEvent expose;
      SDL_QuitEvent quit;
      SDL_UserEvent user;
      SDL_SysWMEvent syswm;
    } SDL_Event;

Each of the individual elements of this UNION are created as a normal
structure, ie:

       -- C++ --                     -- gforth --

    typedef struct{          struct
      Uint8 type;              byte% field SDL_Active->type
      Uint8 gain;              byte% field SDL_Active->gain
      Uint8 state;             byte% field SDL_Active->state
    } SDL_ActiveEvent;       end-struct SDL_ActiveEvent%

and

       -- C++ --                     -- gforth --

    typedef struct{          struct
      Uint8 type;              byte% field SDL_MouseMotion->type
      Uint8 state;             byte% field SDL_MouseMotion->state
      Uint16 x, y;             word% field SDL_MouseMotion->x
                               word% field SDL_MouseMotion->y
      Sint16 xrel, yrel;       word% field SDL_MouseMotion->xrel
                               word% field SDL_MouseMotion->yrel
    } SDL_MouseMotionEvent;  end-struct SDL_MouseMotionEvent%

When looking through all of the individual structure that make up
the SDL_Event UNION, the first element of each is the field:

      Uint8 type;              byte% field SDL_xxxx->type

This field is used by the program to determine which event type has
occurred.

The data following this field then contains the information that is
relevant to the specific event that has occurred.

The construction of the SDL_Event UNION is therefore very simple, as
it has to contain the initial 'Uint8 type;' field, followed by a data
space that is large enough to handle the largest structure type of
the elements that make up the overall UNION construct.

                          --- Concept ---

What I think needs to be created to handle the creation of UNIONS in
gforth are the following functions:

    union
    ufield
    end-union

which would be counterparts to the structure definition functions:

    struct
    field
    end-struct

and would make the gforth definition of the SDL_Event construct:

    union
      SDL_ActiveEvent%      ufield
      SDL_KeyboardEvent%    ufield
      SDL_MouseMotionEvent% ufield
      SDL_MouseButtonEvent% ufield
      SDL_JoyAxisEvent%     ufield
      SDL_JoyBallEvent%     ufield
      SDL_JoyHatEvent%      ufield
      SDL_JoyButtonEvent%   ufield
      SDL_ResizeEvent%      ufield
      SDL_ExposeEvent%      ufield
      SDL_QuitEvent%        ufield
      SDL_UserEvent%        ufield
      SDL_SysWMEvent%       ufield
    end-union SDL_Event%

In the following UNION definition, the ufield code will NOT be
creating an entry for each field.  Rather, ufield will perform a
comparison between the previous size and the current size to find
which is larger, leaving the largest size on the stack for the next
ufield entry to process.

    : union ( -- ) 1 0 ;

    : ufield ( palign psize nalign nsize -- maxalign maxsize )
      2 pick over > if
        2DROP
      else
        rot drop rot drop
      then
    ;

    : end-union ( align size "name" -- )
      over nalign \ pad size to full alignment
      2CONSTANT
    ;

                           --- Usage ---

In use, the above SDL_Event% UNION can be allotted in memory as:

    SDL_Event% %allot CONSTANT Event

and then populated during program execution via the SDL_PollEvent
function:

    Event SDL_PollEvent

The program would then process the information in the structure, if
an Event had occurred.

  : Process-Keys ( -- )
    begin
      Event SDL_PollEvent                   \ while there is an event
    while
      Event SDL_Event->type C@                   \ get the event type
      case                                     \ and process the type
        SDL_USERQUIT of ... endof
        SDL_KeyDown  of ... endof
        SDL_KeyUp    of ... endof
      endcase
    repeat
  ;

                          --- Concerns ---

At the moment, ufield is not keying on the size of the palign/nalign
data, only on the psize/nsize data.  I am not certain as to whether a
larger alignment value needs to be kept, as is being done with the
largest size value.

                        --- Limitations ---

I have not attempted to perform an in-depth study into the needs and
requirements for implementing a fully-functional UNION construction.
Rather, I have only concentrated on the requirements to get the SDL
data space to be formatted correctly.

There are likely many things missing.

However, for the moment, this simple method of creating UNIONs is all
that is required for the functionality required by the SDL definition
that is listed above.

============================= Test Code ====================== [THEN]

[IFDEF] ---marker---
  ---marker---
[ENDIF]

marker ---marker---

decimal

unused constant  free-dictionary-memory
utime  2constant compilation-start-time

\ ---[ Define the aligned field sizes ]------------------------------

[IFUNDEF] cell%   4 4 2constant cell%  [ENDIF]
[IFUNDEF] ptr%    4 4 2constant ptr%   [ENDIF]
[IFUNDEF] int%    4 4 2constant int%   [ENDIF]
[IFUNDEF] word%   2 2 2constant word%  [ENDIF]
[IFUNDEF] byte%   1 1 2constant byte%  [ENDIF]
[IFUNDEF] zptr%   1 0 2constant zptr%  [ENDIF]

\ ---[ Define the UNION functions ]----------------------------------

: union ( -- ) 1 0 ;

: ufield ( palign psize nalign nsize -- maxalign maxsize )
  2 pick over > if
    2DROP                     \ if psize>nsize then drop nalign/nsize
  else
    rot drop rot drop                        \ else drop palign/psize
  then
;

: end-union ( align size "name" -- )
  over nalign                            \ pad size to full alignment
  2CONSTANT
;

\ ---[ Define the Structures ]---------------------------------------

struct
  byte% field SDL_Event->type
  1 0   field SDL_Event->payload
end-struct SDL_EventType%

struct
  byte% field SDL_JoyAxis->type
  byte% field SDL_JoyAxis->which
  byte% field SDL_JoyAxis->axis
  word% field SDL_JoyAxis->value
end-struct SDL_JoyAxisEvent%

struct
  byte% field SDL_JoyBall->type
  byte% field SDL_JoyBall->which
  byte% field SDL_JoyBall->ball
  word% field SDL_JoyBall->xrel
  word% field SDL_JoyBall->yrel
end-struct SDL_JoyBallEvent%

struct
  byte% field SDL_JoyHat->type
  byte% field SDL_JoyHat->which
  byte% field SDL_JoyHat->hat
  byte% field SDL_JoyHat->value
end-struct SDL_JoyHatEvent%

struct
  byte% field SDL_JoyButton->type
  byte% field SDL_JoyButton->which
  byte% field SDL_JoyButton->button
  byte% field SDL_JoyButton->state
end-struct SDL_JoyButtonEvent%

struct
  char% field SDL_Keysym->scancode
  int%  field SDL_Keysym->sym
  int%  field SDL_Keysym->mod
  word% field SDL_Keysym->unicode
end-struct SDL_Keysym%

struct
  byte% field SDL_Active->type
  byte% field SDL_Active->gain
  byte% field SDL_Active->state
end-struct SDL_ActiveEvent%

struct
  byte% field SDL_Keyboard->type
  byte% field SDL_Keyboard->which
  byte% field SDL_Keyboard->state
  SDL_Keysym% field SDL_Keyboard->keysym
end-struct SDL_KeyboardEvent%

struct
  byte% field SDL_MouseMotion->type
  byte% field SDL_MouseMotion->which
  byte% field SDL_MouseMotion->state
  word% field SDL_MouseMotion->x
  word% field SDL_MouseMotion->y
  word% field SDL_MouseMotion->xrel
  word% field SDL_MouseMotion->yrel
end-struct SDL_MouseMotionEvent%

struct
  byte% field SDL_MouseButton->type
  byte% field SDL_MouseButton->which
  byte% field SDL_MouseButton->button
  byte% field SDL_MouseButton->state
  word% field SDL_MouseButton->x
  word% field SDL_MouseButton->y
end-struct SDL_MouseButtonEvent%

struct
  byte% field SDL_Resize->type
  int%  field SDL_Resize->width
  int%  field SDL_Resize->height
end-struct SDL_ResizeEvent%

struct
  byte% field SDL_Expose->type
end-struct SDL_ExposeEvent%

struct
  byte% field SDL_Quit->type
end-struct SDL_QuitEvent%

struct
  byte% field SDL_User->type
  int%  field SDL_User->code
  ptr%  field SDL_User->data1
  ptr%  field SDL_User->data2
end-struct SDL_UserEvent%

struct
  byte% field SDL_SysWM->type
  ptr%  field SDL_SysWM->msg
end-struct SDL_SysWMEvent%

\ ---[ Define the UNION ]--------------------------------------------

union
  SDL_ActiveEvent%      ufield
  SDL_KeyboardEvent%    ufield
  SDL_MouseMotionEvent% ufield
  SDL_MouseButtonEvent% ufield
  SDL_JoyAxisEvent%     ufield
  SDL_JoyBallEvent%     ufield
  SDL_JoyHatEvent%      ufield
  SDL_JoyButtonEvent%   ufield
  SDL_ResizeEvent%      ufield
  SDL_ExposeEvent%      ufield
  SDL_QuitEvent%        ufield
  SDL_UserEvent%        ufield
  SDL_SysWMEvent%       ufield
end-union SDL_Event%

page

free-dictionary-memory unused -
.( Compilation Size: ) . .( bytes)
utime compilation-start-time d- 1 1000 m*/
cr .( Compilation Time: ) d. .( msec) cr

.( Sizing of the SDL_Event% components) cr
.( [First value is alignment; second value is size]) cr
cr
.(     SDL_JoyAxisEvent% : ) SDL_JoyAxisEvent%     swap . . cr
.(     SDL_JoyBallEvent% : ) SDL_JoyBallEvent%     swap . . cr
.(      SDL_JoyHatEvent% : ) SDL_JoyHatEvent%      swap . . cr
.(   SDL_JoyButtonEvent% : ) SDL_JoyButtonEvent%   swap . . cr
.(      SDL_ActiveEvent% : ) SDL_ActiveEvent%      swap . . cr
.(    SDL_KeyboardEvent% : ) SDL_KeyboardEvent%    swap . . cr
.( SDL_MouseMotionEvent% : ) SDL_MouseMotionEvent% swap . . cr
.( SDL_MouseButtonEvent% : ) SDL_MouseButtonEvent% swap . . cr
.(      SDL_ResizeEvent% : ) SDL_ResizeEvent%      swap . . cr
.(      SDL_ExposeEvent% : ) SDL_ExposeEvent%      swap . . cr
.(        SDL_QuitEvent% : ) SDL_QuitEvent%        swap . . cr
.(        SDL_UserEvent% : ) SDL_UserEvent%        swap . . cr
.(       SDL_SysWMEvent% : ) SDL_SysWMEvent%       swap . . cr
cr .( Final size of the SDL_Event% UNION ) cr cr
.(            SDL_Event% : ) SDL_Event%            swap . . cr

\ =============================================[End gforth UNIONs]===
