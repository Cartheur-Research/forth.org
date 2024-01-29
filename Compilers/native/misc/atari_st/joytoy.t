\ File "JoyToy.t"\  Simple demo of joysticks on ST\       Copyright (c) 1987 by Greg Guerin.\       Use freely, as long as copyright notice remains.\ Using only Joystick 1, a small crosshair is moved around the screen.\ The crosshair shape is made by drawing two intersecting lines.  The\ line-drawing is not terribly fast, so there is a flickering effect\ when the crosshair moves.  Screen-switching or faster drawing will\ eliminate this flicker.anew JoyToy-Markerdecimalinclude Joystick.t      \ need joystick words: Ding  ( -- )  7 emit  ;       \ ascii BEL character: ClipOn  ( -- ) \ clip drawing to physical screen         0 0        XYPixels  1 1 -point        TRUE VClipRect  ;: DrawIt  ( -- )  \ the shape is a crosshair        -10 0 RMoveTo           \ left 10         20 0 RDrawTo           \ right 20        -10 10 RMoveTo          \ up 10, left 10          0 -20 RDrawTo         \ down 20          0  10 RMoveTo  ;      \ up 10 returns to original loc'n\ In the interest of speed, decode the 16 possible joystick states\ directly to one of 16 X & Y deltas.  Although not all 16 values\ are possible joystick states, we put them all in the table anyway.\ The "impossible" values are marked with a "*" in their comments.\ If the mouse is connected to Port 0 when joystick monitoring begins,\ some of the "impossible" values may actually be generated.\ Use the fact that @point takes the first word as Y, the second as X.create Directions       \ X  Y         0  0  w, w,    \   0: no dX or dY         0  1  w, w,    \   1: up         0 -1  w, w,    \   2: down         0  0  w, w,    \ * 3: up + down        -1  0  w, w,    \   4: left        -1  1  w, w,    \   5: left + up        -1 -1  w, w,    \   6: left + down        -1  0  w, w,    \ * 7: left + up + down         1  0  w, w,    \   8: right         1  1  w, w,    \   9: right + up         1 -1  w, w,    \  10: right + down         1  0  w, w,    \ *11: right + up + down         0  0  w, w,    \ *12: right + left         0  1  w, w,    \ *13: right + left + up         0 -1  w, w,    \ *14: right + left + down         0  0  w, w,    \ *15: right + left + up + down         : MoveIt  ( joyState -- )        15 and          \ isolate lower 4 bits          ?dup  if        \ joystick is active                4* Directions + @point  \ get delta X & Y                DrawIt          \ erase the old shape                RMoveTo         \ move to new location                DrawIt          \ ..before showing new shape        then  ;variable LastFire: JoyDecode  ( joyState -- )        dup  MoveIt        \ if bit set, then fire-button was pressed        128 and  if                LastFire @ not if                        \ was up last time, now down                        Ding                        LastFire on                then        else                \ fire-button is now up                LastFire off        then  ;: JoyToy  ( -- )        CS              \ clear the screen        CursorOff       \ hide the blinking text cursor        3 VDrawingMode  \ XOR drawing mode        ClipOn          \ clip drawing to screen        GInit           \ init the user graphics        Center          \ center the user coordinates        Cartesian ON    \ positive Y is up        0 0 MoveTo      \ go to origin        DrawIt          \ draw initial shape                JoyBegin        \ let IKBD monitor joysticks        begin                1 JoyStick      \ read the joystick state                 JoyDecode      \ move cursor accordingly        ?Terminal until         \ ..until a key is pressed        JoyEnd          \ resume monitoring mouse on Stick 0 port        CursorOn  ;     \ show cursor again