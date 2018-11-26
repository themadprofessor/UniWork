CC=clang
CFLAGS+=-Wall -Werror -pedantic-errors -pthread -g -O1 -fsanitize=thread

dependencyDiscoverer: dependencyDiscoverer.o tsllist.o tshtable.o
	${CC} ${CFLAGS} -o $@ $?

clean:
	rm *.o
