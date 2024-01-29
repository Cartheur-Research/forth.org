
Forth Interest Group
Category 18,  Topic 21
Message 4         Wed Jan 31, 1990
GARY-S                       at 08:04 EST
 
             
  PORTED FROM UseNet =>
              ------

 From: wmb@SUN.COM (Mitch Bradley)

 Subject: Re: SCHEME in Forth
 Message-ID: <9001301425.AA25919@jade.berkeley.edu>
 Date: 29 Jan 90 17:43:19 GMT
 Sender: daemon@ucbvax.BERKELEY.EDU
 Reply-To: <FIGI-L%SCFVM.bitnet@jade.berkeley.edu>
 Organization: The Internet

 > Can you provide an address and the name of a contact person
 > for CAMP so readers can follow up on SCHEME in Forth and
 > perhaps obtain a copy.

 The head of the CAMP group is Dr. Rupert Nieberle.

 The author of SCHEME in Forth is Markus Freericks.

 CAMP
 Technische Universitat Berlin H51
 Strasse des 17. Juni 135
 D-1000 Berlin-12
 West Germany

 Markus Freericsks' email address is mfx@tubopal.cs.tu-berlin.de
 His personal mailing address is

 Oranienburger Strasse 142
 1000 Berlin 26
 West Germany


 The name of the Forth SCHEME implementation is YLEM.  (phonetically,
 that's pronounced oo-lim, oo as in "food").

 Mitch
----------
Forth Interest Group
Category 18,  Topic 21
Message 7         Fri Feb 16, 1990
GARY-S                       at 06:54 EST
 
             
  PORTED FROM UseNet =>
              ------

 From: gintera@cpsc.ucalgary.ca (Andrew Ginter)

 Subject: Re: SCHEME in Forth
 Summary: YLEM summary
 Message-ID: <2504@cs-spool.calgary.UUCP>
 Date: 15 Feb 90 16:30:40 GMT
 References: <454.UUL1.3#5129@willett.UUCP>
 Sender: news@calgary.UUCP

 I asked the folks in Berlin about YLEM and this is what they
 sent back.

 Andrew Ginter, 403-282-2984, gintera@CPSC.UCALGARY.CA, Ginter@UNCAMULT.BITNET

 ================================================================
 Hi!

 (This is the standard reply giving a rough description of YLEM, our
 Scheme-in-Forth.)

 Thank you for showing interest in YLEM.

 We, the CAMP group at the TU Berlin, are developing a soft- and
 hardware-base for computer-aided music composition work.  Our hardware
 mainly consists of a number of Atari-ST machines connected by our own
 kind-of-SCSI network and a few gadgets like a DSP56000 card.

 Most of our programming work is done with FORMULA, which is a very
 nice FOrth-based MUsic LAnguage written by Dave Anderson and Ron
 Kuivila. Underlying this is FORTHMACS, Mitch Bradley's Forth
 implementation.

 YLEM is an implementation of Scheme written in the context of FORMULA
 with real-time music performance in mind, which implies:

 - YLEM is running in real-time
 - the garbage collector is running in real-time
 - multiple FORMULA-processes can run YLEM simultaneously
 - YLEM is highly optimized: about 50% of the central interpreter
   is done in 68000 Assembler
 - YLEM coexists with FORMULA, meaning: you can evaluate an
   expression  within a Forth word, and you can call a Forth like
   a primitive Scheme function (in fact, primitive functions *are*
   simply Forth words residing wihtin a special vocabulary)
 - YLEM is totally un-portable.

 All this describes YLEM 0.9+, which is the language we are using well,
 nearly dayly, now. YLEM 0.9+ is still a little buggy (as the number
 implies) but pretty stable.

 Currently, work on Version 2.0 has stopped, which shall be cleaner and
 written in nearly standard-Forth. A small compilation facility shall
 be installed instead of the quite extensive assembler optimizations of
 the old (0.9+) interpreter core, and all FORMULA- dependent parts
 shall be isolated wihtin a small region.

 I, being only a humble student, have not the time to complete 2.0 just
 now. Of course, everybody interested in YLEM 0.9+ may get a copy on
 disk, complete with a FORMULA and a documentation (written in German -
 sorry!).

 My Address is:

 Markus Freericks        (mfx@opal.cs.tu-berlin.de)
 Oranienburger Str. 142
 1000 Berlin 26
 WEST-GERMANY
 phone: (030)-4034110

 For information about FORMULA, contact
 Dave Anderson   (should be anderson@Berkeley.EDU) or
 Ron Kuivila

 For information about FORTHMACS, contact
 Mitch Bradley   (wmb@SUN.COM)

----------


