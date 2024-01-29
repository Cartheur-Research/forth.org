
Forth Interest Group
Category 18,  Topic 73
Message 2         Wed Apr 04, 1990
GARY-S                       at 08:12 EDT
 
             
  PORTED FROM UseNet =>
              ------

 From: koopman@a.gp.cs.cmu.edu (Philip Koopman)
 Subject: Re: Forth Blocks <-> ASCII files
 Summary: try this out...
 Keywords: Forth Screens
 Message-ID: <8692@pt.cs.cmu.edu>
 Date: 2 Apr 90 11:25:50 GMT
 References: <209@rafos.UUCP>
 Organization: Carnegie-Mellon University, CS/RI

 In article <209@rafos.UUCP>, skip@rafos.UUCP (Skip Carter) writes:
 >       Is there a utility around that will allow me to take a file
 > that is in Forth screen format and convert it to vanilla ASCII ?

 /* Convert block file to text file.  By Phil Koopman Jr. */
 /* Written in Turbo C 2.0 for the IBM PC */
 /* It's free -- you get what you pay for! */
 /*   last update: 10/27/89  */

 /* Purpose:  Inputs DOS block format files, and places into an
  *           ASCII text file format with a blank line between screens.
  * Usage:  takes standard input and produces standard output
  *           blocks < infile > outfile
  * I use a text editor to strip trailing blanks.
  */

 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>

 char buffer[256];     /* one line output buffer */

 void main()
 { int lines ;
    lines = 0 ;
    while ( !feof(stdin) )
    { fgets(buffer,65,stdin);
      *(buffer+64) = '\n' ;
      *(buffer+65) = '\0' ;
      if ( ++lines > 15 )
       { lines = 0 ;
         *(buffer+65) = '\n' ;
         *(buffer+66) = '\0' ;
       }
      fputs(buffer, stdout);
    }
 }

   Phil Koopman                koopman@greyhound.ece.cmu.edu   Arpanet
   2525A Wexford Run Rd.
   Wexford, PA  15090
 Senior Scientist at Harris Semiconductor, adjunct professor at CMU.
 I don't speak for them, and they don't speak for me.
----------
Forth Interest Group
Category 18,  Topic 73
Message 3         Wed Apr 04, 1990
GARY-S                       at 08:14 EDT
 
             
  PORTED FROM UseNet =>
              ------

 From: hiebeler@cs.rpi.edu (Dave Hiebeler)
 Subject: Re: Forth Blocks <-> ASCII files
 Message-ID: <%$K#BP$@rpi.edu>
 Date: 2 Apr 90 17:56:03 GMT
 References: <209@rafos.UUCP>
 Organization: RPI CS Dept, and LANL Center for Nonlinear Studies

   I work with the CAM-6 (and more recently, CAM-PC) line of Cellular
 Automata Machines, which is controlled by software written in Forth.
 (Send me a note if you're interested in these CAMs, they are plug-in
 boards for IBM PCs that give you Cray-1 speeds for doing cellular
 automata simulatios).

   I threw these two programs together a while ago to convert between
 Forth block files and normal text files.  I've used these programs on
 Unix (Suns), and IBM PCs using Turbo-C.  There isn't anything really
 non-portable in there though, so they should work with just about any
 C compiler, I hope.

   The files are separated by a line of "="s; search for that to break
 them apart.  Also be sure to remove my .signature at the end.

 ===== First file: 4th2text.c =====
 /*
  * File: 4th2text.c
  *   By: Dave Hiebeler
  *       August 1989
  *
  * Copyright 1990 David Hiebeler and Automatrix, Inc.
  * This code can be freely copied and modified for personal use, provided
  * the copyright notice is retained. This code cannot be sold for profit.
  *
  * This program reads in a standard Forth "screen" file (e.g. a .EXP
  * or .4TH file) which is arranged in 64x16 blocks of characters.
  * It simply inserts a line-feed after every 64th character, thus
  * turning it into a file that can be edited by normal means.
  * It will also truncate trailing spaces on each line, to make the resulting
  * file smaller to conserve disk space.
  *
  * If you use the "-n" flag, it will put the string "|x.y|" at the beginning
  * of each line, where "x" is the block number, and "y" is the line number
  * within that block.
  *
  * Usage: 4th2text [-n] infile outfile
  * If infile or outfile is "-", then stdin or stdout will be used,
  * respectively.
  */

 #include <stdio.h>

 void
 printusage(s)
 char *s;
 {
     fprintf(stderr, "Usage: %s [-n] infile outfile\n",s);
     exit(1);
 }


 void
 quit(s)
 {
     fprintf(stderr,"%s\n",s);
     exit(1);
 }



 main(argc,argv)
 int argc;
 char **argv;
 {
     FILE *infile,*outfile;
     int counter, numbers, line, screen, i;
     char cc, line_buf[64];

     if ((argc != 3) && (argc != 4))
             printusage(argv[0]);
     numbers = 0;
     if (argc == 4) {
         if (strcmp(argv[1], "-n"))
             printusage(argv[0]);
         else
             numbers = 1;
     }
     if (!strcmp(argv[1+numbers], "-"))
         infile = stdin;
     else
         if ((infile=fopen(argv[1+numbers],"r"))==NULL)
             quit("Could not open input-file");
     if (!strcmp(argv[2+numbers], "-"))
         outfile = stdout;
     else
         if ((outfile=fopen(argv[2+numbers],"w"))==NULL)
             quit("Could not open output-file");
     line = 0;
     screen = 0;
     counter = 0;
     while ((cc=getc(infile))!=EOF) {
         if ((!counter) && numbers) {
             if (line < 10)
                 fprintf(outfile, "| %d.%d|", screen, line);
             else
                 fprintf(outfile, "|%d.%d|", screen, line);
             line++;
             if (line == 16) {
                 line = 0;
                 screen++;
             }
         }
         /* putc(cc,outfile); */
         line_buf[counter] = cc;
         if ((++counter)==64) {
             counter = 0;
             /* putc('\n',outfile); */
             for (i=63; i; i--) {        /* remove trailing spaces */
                 if (line_buf[i] != ' ')
                     break;
                 else
                     line_buf[i] = '\0';
             }
             for (i=0; line_buf[i] != '\0'; i++)
                 putc(line_buf[i], outfile);
             putc('\n', outfile);
         }
     }
     fclose(outfile);
     fclose(infile);
 }

 ============================================================
 ===== Second file: text24th.c =====
 /*
  * File: text24th.c
  *   By: Dave Hiebeler
  *       August 1989
  *
  * Copyright 1990 David Hiebeler and Automatrix, Inc.
  * This code can be freely copied and modified for persona This program reads
in a standard text file and converts it into
  * a Forth "screen" file, where each screen is made of 64x16 characters.
  * Any lines longer than 64 characters are truncated (and a warning is
printed
  * to stderr).
  *
  * If any lines in the file begin with the string "|xx...xx|", that prefix
  * will be stripped.  That is, if the file was generated using "4th2text"
  * with the "-n" option, the screen/line numbers will be removed when
  * converting back to Forth block format.  You can actually put anything,
  * not just digits, between the '|' characters, and it will be dropped.
  *
  * Usage: text24th infile outfile
  * If infile or outfile is "-", then stdin or stdout will be used,
  * respectively.
  */

 #include <stdio.h>

 void
 printusage(s)
 char *s;
 {
     fprintf(stderr, "Usage: %s infile outfile\n",s);
     exit(1);
 }


 void
 quit(s)
 char *s;
 {
     fprintf(stderr,"%s\n",s);
     exit(1);
 }


 void
 quitd(s,d)
 char *s;
 int d;
 {
     fprintf(stderr, s, d);
     exit(1);
 }


 main(argc,argv)
 int argc;
 char **argv;
 {
     FILE *infile,*outfile;
     int counter, done, eof, line;
     char cc;
     int i,j;

     if (argc != 3)
             printusage(argv[0]);
     if (!strcmp(argv[1], "-"))
         infile = stdin;
     else
         if ((infile=fopen(argv[1],"r"))==NULL)
             quit("Could not open input-file");
     if (!strcmp(argv[2], "-"))
         outfile = stdout;
     else
         if ((outfile=fopen(argv[2],"w"))==NULL)
             quit("Could not open output-file");
     eof = 0;
     counter = 0;
     line = 0;
     while ((!eof) && ((cc=getc(infile))!=EOF)) {
         if ((cc == '|') && (!counter)) {        /* '|' at beginning of line
*/
             while ((cc=getc(infile)) != '|') {
                 if (cc==EOF) {
                     eof = 1;
                     break;
                 }
                 if ((cc == '\n') || (cc == '\r'))
                     quitd("Error -- umatched '|' enountered on line
%d",line);
             }
         }
         else if ((cc == '\n') || (cc == '\r')) {
             line++;
             for (i=0; i < (64-counter); i++)
                 putc(' ',outfile);
             counter = 0;
         }
         else {
             putc(cc,outfile);
             if ((++counter)==64) {
                 line++;
                 counter = 0;
                 done = 0;
                 while (!done) {
                     cc = getc(infile);
                     if ((cc == '\n') || (cc == '\r'))
                         done = 1;
                     else if (cc == EOF) {
                         done = 1;
                         eof = 1;
                     }
                     else counter = 1;
                 }
                 if (counter) {
                     fprintf(stderr, "Line %d too long, was
truncated\n",line);
                     counter = 0;
                 }
             }
         }
     }
     if (line%16) {
         for (i=0; i < 16-(line%16); i++)
             for (j=0; j<64; j++)
                 putc(' ',outfile);
     }
     fclose(outfile);
     fclose(infile);
 }

 ============================================================
 -- 
 Dave Hiebeler / Computer Science Dept. / Amos Eaton Bldg. /
 Rensselaer Polytechnic Institute / Troy, NY 12180-3590 USA
 Internet (preferred): hiebeler@turing.cs.rpi.edu   Bitnet: userF3JL@rpitsmts
 "Off we go, into the wilds you ponder..."
----------
Forth Interest Group
Category 18,  Topic 73
Message 4         Wed Apr 11, 1990
GARY-S                       at 06:23 EDT
 
             
  PORTED FROM UseNet =>
              ------

 From: jax@well.sf.ca.us (Jack J. Woehr)
 Subject: Re: Forth Blocks <-> ASCII files
 Keywords: Forth Screens
 Message-ID: <17010@well.sf.ca.us>
 Date: 3 Apr 90 02:47:47 GMT
 References: <209@rafos.UUCP>

 skip@rafos.UUCP (Skip Carter) writes:


 >       Is there a utility around that will allow me to take a file
 >that is in Forth screen format and convert it to vanilla ASCII ?
 >How about going the other way ?
 >       I am interested in this problem in general, but if it helps
 >my immediate need is to solve this with F-83 on a PC.

         Skip, here it is in C for the Amiga, pretty easy to
 translate, if ya asks me to uSoft C or the like. Have fun!

 ------------------------------------------------------------------

 /* unblock.c ... convert a named FORTH blockfile to text on stdout *
  * jack j. woehr jax@well.UUCP JAX on GEnie                        *
  * SYSOP, RealTime Control & Forth Board, (303) 278-0364 3/12/24   *
  * USAGE: 1> UNBLOCK foo.blk >foo.txt                              *
  */

 #include <INCLUDE:lattice/stdio.h>
 #include <INCLUDE:lattice/fcntl.h>
 #define READLENGTH 64

 main(argc,argv)
 int argc;
 char *argv[];

 {

         int status;
         int file;

         char a_line[66];
         int backptr;

         if (argc < 2)
                 {

         if ((file = open(argv[1], O_RDONLY)) == -1)
                 {
                 fprintf(stderr,"Couldn't open %s\n", argv[1]);
                 exit(1);
                 }

         a_line[0]  = NULL;
         a_line[65] = NULL;

         while ((status = read(file, &a_line[1], READLENGTH)) >0)
                 {
                 backptr = 64;
                 while(a_line[backptr--] == 0x20)
                         a_line[(backptr+1)] = NULL;
                 printf("%s\n",&a_line[1]);
                 }
         status = close(file);
         exit(status);
 }

 /*
  jax@well     ." Sysop, Realtime Control and Forth Board"      FIG     
  jax@chariot  ." (303) 278-0364 3/12/2400 8-n-1 24 hrs."     Chapter    
  JAX on GEnie       ." Tell them JAX sent you!"             Coordinator 
  jax@well.sf.ca.us   Now Starting to Attend ANSI X3J14   and going bald 
 */
----------


