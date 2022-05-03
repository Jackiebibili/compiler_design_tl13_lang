#!/usr/bin/make -f

CC = gcc
# CFLAGS = -Wall -I/Users/jack-li/Documents/spring_2022/compiler_design/project/lib/uthash/include	# Path to include library
CFLAGS = -std=c99 -Wall -I/home/012/j/jx/jxl180095/compiler_design/lib/uthash/include	# Path to include library

# Flags for the Lex implicit rules
LEX = /usr/bin/flex
LFLAGS = 

# Implicit Makefile rule to use YACC for C programs
YACC = /usr/bin/bison
YFLAGS = -dy

EXECFILE = compiler
OBJS = parse.o scan.o gen.code.o

.PRECIOUS: scan.c parse.c

all: $(EXECFILE)

clean:
	rm -f $(OBJS) $(EXECFILE) y.tab.h parse.c scan.c *~ \#*

$(EXECFILE):	$(OBJS)
	$(CC) -o $@ $(OBJS)
