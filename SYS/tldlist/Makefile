CC=clang
CCFLAGS=-Wall -Werror -g -pedantic-errors

tldmonitor: tldmonitor.o date.o tldlist.o
	$(CC) $(CCFLAGS) -o tldmonitor tldmonitor.o date.o tldlist.o

test: test.o date.o
	$(CC) $(CCFLAGS) -o test test.o

test.o: test.c
	$(CC) $(CCFLAGS) -o test.o -c test.c

date.o: date.h date.c
	$(CC) $(CCFLAGS) -o date.o -c date.c

tldlist.o: tldlist.h tldlist.c
	$(CC) $(CCFLAGS) -o tldlist.o -c tldlist.c

tldmonitor.o: tldmonitor.c date.h tldlist.h
	$(CC) $(CCFLAGS) -o tldmonitor.o -c tldmonitor.c

clean:
	rm -f *.o tldmonitor test
