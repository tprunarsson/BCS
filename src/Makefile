CC = gcc

#
LIBS =

#
SRCS = \
	covid.c \
	rndlib.c \
    	simlib.c \
	utils.c \
	fileio.c
#
OBJS = \
	covid.o \
	rndlib.o \
    	simlib.o \
	utils.o \
	fileio.o

# -g
DEBUGFLAG = -Wall

INCLUDEDIRS =

HDRS =


# linux

CFLAGS = $(INCLUDEDIRS) $(DEBUGFLAG)
LDLIBS = -lm -ldl
LDFLAGS =

#
PROGRAM =	covid

$(PROGRAM):	$(OBJS) $(LIBS)
		$(CC) $(CFLAGS) $(OBJS) $(LDFLAGS) $(LDLIBS) -o $(PROGRAM)
		@echo type ./$(PROGRAM) to run.

covid.o:	covid.c covid.h simlib.h rndlib.h fileio.h
		$(CC) -c $(CFLAGS) covid.c

simlib.o:	simlib.c simlibdefs.h 
		$(CC) -c $(CFLAGS) simlib.c

rndlib.o:	rndlib.c 
		$(CC) -c $(CFLAGS) rndlib.c

fileio.o:	fileio.c 
		$(CC) -c $(CFLAGS) fileio.c

utils.o:	utils.c 
		$(CC) -c $(CFLAGS) utils.c

clean:
		rm *.o
		rm $(PROGRAM)
