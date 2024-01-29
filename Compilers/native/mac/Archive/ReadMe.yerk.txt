Yerk 3.6.2 release notes:

This is a release of Yerk. The restrictions on its use may not
quite qualify as public domain, and below is a copy of the release statement
sent to me by Kriya Systems, the company that marketed Yerk when it was
known as Neon.

  "Kriya Systems, Inc. gives you [me] the permission to freely distribute for
  scientific and educational purposes the programming language formerly known
  as Neon, including the distribution of the source which has been released
  to you.  You do not have the right to use the name Neon, as it apparently
  had prior use by another company and is not a valid trademark of Kriya
  Systems.  All commercial distribution rights are reserved by Kriya Systems,
  Inc."

That last sentence seems to have some people confused as to whether this
is really in the public domain. I'll see about clarifying that with
Kriya. As it stands now, that paragraph is the basis of this distribution
of Yerk, and anyone who uses it should abide by it.

Yerk is an object oriented language based on a Forth Kernel with some major
modifications.  It was originally known as Neon, developed and sold as a product
by Kriya Systems from 1985 to 1989. Several of us at The University of Chicago
have maintained Yerk since its demise as a product. Because of the possible
trademark conflict that Kriya mentions, we picked the name Yerk, which is at
least not an acronym for anything, but rather stands for Yerkes Observatory,
part of the Department of Astronomy and Astrophysics at U of C.

The manual has been updated to reflect most updates and changes from the last
release of Neon 2.0 to version of Yerk 3.3.2.  The current version of Yerk is
now up to 3.6.2, and runs with all systems up to 7.0.1. There are only minor changes
to the manual. I'm sure that some things are not explained correctly, omitted,
or are just plain wrong.  I would appreciate receiving any comments, corrections or
ideas concerning the manual or the application. I hope to take care of updating
the manual sometime in january. In the meantime, check the modifications file to
see what has changed and hints.

In addition to the example sources that Kriya issued with Neon are some examples
of my own, just to show how to use some of the additional classes of Yerk. Check
the supplement folder

If people are interested, I have less general classes for color quickdraw
interfaces, MacTCP classes, and a host of others. These were written for
specific applications, but I could make them available to people who contact
me.

There are a few bugs in the assembler: movem, eor, and extended arithmetic
instructions.

Some features of the language are:

   - defaulted early binding, with ability to late bind in almost any
      circumstance
   - inheritance (not multiple)
   - floating point (SANE)
   - many system classes and objects for mac interfacing:
      windows, controls, events, files, arrays, ordered-columns, menus,
      hierarchical and popup menus, handles, strings, mouse, quickdraw,
      modal dialogs, offscreen bitmaps, vbl, time manager, etc.
   - module (overlay) creation that are loaded only when necessary and
      may be purged from application heap memory.

Some forth extensions are:

   - local input parameters
   - named input variables
   - multiple cfa words (including vectors and values)
   - CASE
   - SELECT
   - 68000 assembler

Yerk 3.6.2 is 32 bit clean, but does not respond to high level events. I don't see
a need for the Yerk environment itself to handle high level events, but any
application that is written in Yerk CAN handle the events...it's up to the
programmer. I left hooks in for such handling and one should look in system
source EVENTS.

We have used the language on the following macs:
mac+,se,se30, all mac II's, portables, powerbooks and Quadras. There
is no reason to believe that it won't work on any mac+ or beyond. Any
system >= 6.0 is recommended.

If anyone has any questions, feel free to contact me:

Bob Loewenstein
Dept. of Astronomy and Astrophysics
University of Chicago
Yerkes Observatory
Williams Bay, Wisconsin  53191
414-245-5555

rfl@oddjob.uchicago.edu
