valgrind --leak-check=full --track-origins=yes ./tldmonitor 01/01/2000 01/09/2013 <small.txt | sort -n | diff - small.out
