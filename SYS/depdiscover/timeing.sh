#!/bin/bash
rm -f crawlers.txt
make > /dev/null
for i in 1 2 4 8; do
	echo "$i crawlers" >> crawlers.txt
	(CRAWLER_THREADS=${i} /usr/bin/time -p ./dependencyDiscoverer -Itest test/*.c test/*.l test/*.y > /dev/null) &>> crawlers.txt
	echo >> crawlers.txt
done
