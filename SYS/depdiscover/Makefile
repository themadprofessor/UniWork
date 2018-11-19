CC=clang
CFLAGS+=-Wall -Wextra -Werror -pedantic-errors -Wformat=2 -wno-unused-parameter -Wshadow -Wwrite-strings\
 -Wstrict-prototypes -Wnested-externs -Wmissing-include-dirs -pthread -std=c99

dependencyDiscoverer: dependencyDiscoverer.o tsllist.o tshtable.o
	${CC} ${CFLAGS} -o $@ $?

clean:
	rm *.o