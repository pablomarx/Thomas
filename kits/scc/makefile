# This file compiles the extensions to the 01nov91jfb release of Scheme->C.
# $Id: scc_makefile,v 1.4 1992/09/23 17:58:00 birkholz Exp $

# Defaults:

.SUFFIXES:
.SUFFIXES: .sc .c .o

# Directory containing the source for the 01nov91jfb release of Scheme->C:

SCRTDIR = /crl/src/schemetoc/src

# Currently installed 01nov91jfb Scheme->C compiler:

SCC = /usr/local/bin/scc

# Currently installed 01nov91jfb Scheme->C compiler library:

SCCLIB = /usr/local/lib/schemetoc/libsc.a

CFLAGS = -O -I${SCRTDIR}/scrt
SCFLAGS =

OBJS = aftergc.o callcc.o dynwind.o heap.o main.o mit.o scinit.o scc-specific.o

.c.o:
	${CC} -c ${CFLAGS} -D${cpu} -I. $*.c

.sc.o:
	${SCC} -c ${SCFLAGS} ${CFLAGS} $*.sc

mit.sc:	poplat.sc hash.sc record.sc msort.sc

release:
	make noprogs DECstation.o scc.tar.Z

sci:	${OBJS}
	${SCC} -o sci ${SCFLAGS} ${CFLAGS} ${OBJS}	

DECstation.o:	${OBJS} main.o
	ld -r -o DECstation.o ${OBJS} ${SCCLIB}

sci-for-DECstation:
	cc -o sci DECstation.o -lm

scc.tar.Z: ${OBJS} DECstation.o
	rm -f scc.tar.Z
	tar -cf scc.tar DECstation.o README aftergc.sc callcc.c \
		dynwind.sc hash.sc heap.c heap.h implementation-specific \
		main.sc makefile mit.sc msort.sc poplat.sc record.sc \
		scc-rep.scm scc-specific.sc scinit.c
	compress scc.tar

clean:
	rm -f *.o

noprogs:
	rm -f sci
