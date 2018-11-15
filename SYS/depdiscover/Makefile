CC=clang
CFLAGS=-Wall -Werror -pedantic-errors -pthread

dependencyDiscoverer: dependencyDiscoverer.o tsllist.o tshtable.o
	${CC} ${CFLAGS} -o $@ $?

clean:
	rm *.o