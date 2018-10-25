CC=clang
CCFLAGS=-Wall -Werror -g

tldmonitor: tldmonitor.o date.o tldlist.o
	$(CC) $(CCFLAGS) -o tldmonitor tldmonitor.o date.o tldlist.o

date.o: date.h date.c
	$(CC) $(CCFLAGS) -o date.o -c date.c

tldlist.o: tldlist.h tldlist.c
	$(CC) $(CCFLAGS) -o tldlist.o -c tldlist.c

tldmonitor.o: tldmonitor.c date.h tldlist.h
	$(CC) $(CCFLAGS) -o tldmonitor.o -c tldmonitor.c

clean:
	rm -f *.o tldmonitor
