/*
 * File: text24th.c
 *   By: Dave Hiebeler
 *       August 1989
 *
 * Copyright 1990 David Hiebeler and Automatrix, Inc.
 * This code can be freely copied and modified for personal use, provided
 * the copyright notice is retained. This code cannot be sold for profit.
 *
 * This program reads in a standard text file and converts it into
 * a Forth "screen" file, where each screen is made of 64x16 characters.
 * Any lines longer than 64 characters are truncated (and a warning is printed
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
	if ((cc == '|') && (!counter)) {	/* '|' at beginning of line */
	    while ((cc=getc(infile)) != '|') {
		if (cc==EOF) {
		    eof = 1;
		    break;
		}
		if ((cc == '\n') || (cc == '\r'))
		    quitd("Error -- umatched '|' enountered on line %d",line);
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
		    fprintf(stderr, "Line %d too long, was truncated\n",line);
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
