Yerk release notes:

This is a release of Yerk. The restrictions on its use may not quite
qualify as public domain, and below is a copy of the release statement
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
is really in the public domain. What I believe it means is that Yerk can't
be sold. As it stands now, that paragraph is the basis of this
distribution of Yerk, and anyone who uses it should abide by it.

Yerk is an object oriented language based on a Forth Kernel with some
major modifications.  It was originally known as Neon, developed and sold
as a product by Kriya Systems from 1985 to 1989. Several of us at The
University of Chicago have maintained Yerk since its demise as a product.
Because of the possible trademark conflict that Kriya mentions, we picked
the name Yerk, which is at least not an acronym for anything, but rather
stands for Yerkes Observatory, part of the Department of Astronomy and
Astrophysics at U of C.

The manual has been updated to reflect most updates and changes from the
last release of Neon 2.0 to version of Yerk 3.6.7. See the file
'modifications' to get a summary of changes beyond the manual. Yerk runs
on all systems up to 7.5. However, Yerk makes use of certain system 7
additions such as popUp menus, so not all of Yerk will work under system
6.

There are only minor changes to the manual. I'm sure that some things are
not explained correctly, omitted, or are just plain wrong.  I would
appreciate receiving any comments, corrections or ideas concerning the
manual or the application. In the meantime, check the modifications file
to see what has changed and hints.

In addition to the example sources that Kriya issued with Neon are some
examples of my own, just to show how to use some of the additional classes
of Yerk. Check the supplement folder

If people are interested, I have less general classes for color quickdraw
interfaces, MacTCP classes, and a host of others. These were written for
specific applications, but I could make them available to people who
contact me.

Some features of the language are:

   - defaulted early binding, with ability to late bind in almost any
      circumstance
   - objects may be instantiated on the heap (dynamically)
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
   - PATCH
   - 68000 assembler

Yerk is 32 bit clean, but does not respond to high level events. I don't
see a need for the Yerk environment itself to handle high level events,
but any application that is written in Yerk CAN handle the events...it's
up to the programmer. I left hooks in for such handling and one should
look in system source EVENTS.

We have used the language on all macs. There is no reason to believe that
it won't work on any mac+ or beyond. Any system >= 6.0 is usable, but full
compatibility requires system >= 7.0.

For those of you already familiar and using Yerk, read the 'modifications'
file which will tell you how the current release is different from
previous versions. Also, the glossary contains more Yerk words, so you
should also reread it.

FOR THOSE UNFAMILIAR WITH YERK...MAKE SURE YOU DOUBLE-CLICK ON
YERK.COM OR YERKFP.COM, NOT 'YERK'. 'Yerk' is found in the 'yerk folder' and
is the barebones nucleus...no menus or event loop. The '.com' files are
yerk documents, and will load everything you need to start the tutorial.

If anyone has any questions or suggestions, feel free to contact me. Also,
periodic updates can be found on astro.uchicago.edu in directory
/pub/astro/MAC/Yerk.

Bob Loewenstein Dept. of Astronomy and Astrophysics University of Chicago
Yerkes Observatory Williams Bay, Wisconsin  53191 414-245-5555

rfl@yerkes.uchicago.edu
