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
    char line_buf[64];
    int cc;

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
	line_buf[counter] = (char)cc;
	if ((++counter)==64) {
	    counter = 0;
	    /* putc('\n',outfile); */
	    for (i=63; i; i--) {	/* remove trailing spaces */
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
