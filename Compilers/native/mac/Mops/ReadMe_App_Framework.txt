About the Application Framework + Demo
--------------------------------------

Here's version 0.3 of the Application Framework.  It's a long ways from being finished (hence the
"0" primary version number, but is now solid enough to play with - so long as you don't want to do
anything real with it.  It requires Mops 2.5.1 - it uses some of the newer Mops features.  Included is
a demo files that shows how a way to use the App Framework in a "real-world" way.

The App Framework is designed to help take the burden of providing a standard Mac interface away from
the programmer.  Traditionally, the programmer has had to implement the Mac standards: window
dragging, menu selection, support for DA's, etc...  This required a lot of programming and took much
of the programming focus away from the real goal of the programmer - the actual application.

Since much of the Mac interface is standard, it makes sense to have a "template" of sorts that
provides this standard behavior while allowing for the addition of custom behavior.  The programmer
then builds upon this template, adding only the code to make the application unique.  There are many
ways to implement such a template (cut and paste, libraries of functions, include files, etc...), but
Object Oriented Programming provides an elegant solution - class libraries with default methods and
method overriding.

Class libraries with default methods provide the standard behavior.  For example, a window class could
have methods that provide standard dragging and resizing methods.  The programmer then creates a
window object, and the object automatically knows how to drag/resize itself - the programmer does not
need to write this code.

Method overriding allows for customization.  First, if a default method (which provides only standard
behavior) does not perform as the programmer wishes, it can be overridden.  Of course, this requires
the programmer to replace the functionality of the default method, which often defeats the original
purpose of using the template, but it allows for complete customization of any part of the template.
Second, methods can be provided for which there is no standard behavior, yet are often used.  In this
case, the methods simply do nothing or provide minimal support.  This is similar to the idea of
providing "hooks" into existing code, but is easier to implement - the programmer can simply use
overriding, as opposed to using pointers to functions or changing jump vectors.

OK, enough about generalities!  The App Framework is simply a set of classes that represent common Mac
objects - windows, menus, controls, etc... - and some support classes.  Each of the classes has a set
of default methods (which implement the Mac interface as we know it), and some have some methods that
are meant to be overridden.  Some interesting problems had to be solved, and their solutions affect
the look and feel of the App Framework.

First, we need to allow the programmer to alter the built in behavior.  The technique used here is
overriding methods inside sub-classes.  While this leads to a really nice syntax (see below), we need
to realize that we need to use late binding.  This is because we have no idea what code will be used
in our classes - the default or a custom version?  With late binding, we don't cross that bridge until
the last moment.

Second, we need to be able to distinguish between objects created by the App Framework and windows (or
whatever) created directly through the Toolbox.  This is because the Toolbox doesn't understand how
the App Framework works - we don't want to try and send messages to non-objects, as they don't have
methods!  Thus, each of our objects needs to be _registered_, or placed in a list.  One benefit of
this is that we can send messages to the list instead of directly to the object.  The list can then
have some default action to perform if the object isn't found.  An analogy is Apple's SANE software
that does floating-point math in software if an FPU isn't found - the request for floating-point math
is the same in either case.

What We Have and How to Use It
------------------------------

Currently, support for the Mac interface is limited.  However, there are hooks for the entire Mac
interface, they just aren't all developed yet.  As of version 0.3, we have a complete event-loop which
can handle mouse-clicks and key-repeat events, ignoring the rest.

Mouse-clicks are mostly passed onto the registered windows after some preliminary processing, but some
minimal support for menu tracking is also available (this is the preliminary processing that will
occur before passing control to menu objects).  Note that some events don't require action on the part
of the window object - it's constant for window objects and Toolbox windows.  These events are handled
entirely within the event handler.

Of special importance is the key-repeat event (autoKey).  The handler for this is set up as an
"emergency escape."  Holding a key down until it repeats will exit the app.  Of course, a production
version would have a completely different handler!

Using the App Framework is pretty easy.  After including the needed source code via "needs Framework"
and "needs Windows", the classes are available.  There are currently two classes that are of interest:
APP_C and WINDOW_C.

APP_C stands for an application.  It contains the main event loop and the event handlers.  For the
most part, you'll use it pretty much "as is".  The demo uses a slightly modified version where I have
overridden the "nullH:" handler to change its default idle time behavior.

WINDOW_C is the basic window class (duh!).  It contains a minimal amount of window info (the window
pointer), plus handler and helper methods.  The helper methods are automatically called from the APP_C
object - they receive pre-processed version of the events that affect that window.  The helper methods
help the programmer use the window object.  For example, "new:" is used to initialize the window
object, which includes registering it.

To get everything going, a "main:" message needs to be sent to the APP_C object.  See the demo for a
more concrete explanation.

The Demo
--------

This shows the use of the App Framework in a realistic environment.  This demo is not robust, nor is
it an example of good interface methods.  However, it does show how method overridinf is used to make
custom App Framework objects.  It also show how to use these objects once they are created.

The demo simply builds a window and starts running the main event loop.  The event loop automatically
handles mouse clicks (dragging, resizing, closing of windows; tracking menu clicks) and also supports
idle time behavior.  In this case, idle time behavior is tracking a bouncing ball drawn on top of
_everything_, even when the app is running in the background.  That's it!

Note that the demo must be run from within Mops - there's an error that appears only when the app is
installed, and I haven't found it (yet).

Possibilities
-------------

Of course, I'll be adding support for controls, menus, and such in the future (stay tuned).  I also
want to add color window objects (pixmap based), text window objects (TextEdit based), radio button
groups, and other goodies.

While the current App Framework is pretty rough, it's got all the needed hooks - they just need to be
overridden.  If you're a glutton for punishment, go ahead and try to fill in the holes,  However, I'll
be doing the samew thing, so you don't need to if you don't want to.  Above all - enjoy!