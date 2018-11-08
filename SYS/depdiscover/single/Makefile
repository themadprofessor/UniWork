CC=clang
CFLAGS=-W -Wall -Werror -DLINUX -g
OBJECTS=tshtable.o tsllist.o

dependencyDiscoverer: dependencyDiscoverer.o $(OBJECTS)
	$(CC) $(CLFAGS) -o dependencyDiscoverer dependencyDiscoverer.o $(OBJECTS) -lpthread

dependencyDiscoverer.o: dependencyDiscoverer.c tshtable.h tsllist.h
tshtable.o: tshtable.c tshtable.h
tsllist.o: tsllist.c tsllist.h

clean:
	rm -f *.o dependencyDiscoverer *~
