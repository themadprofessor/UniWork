cd test
../dependencyDiscoverer *.l *.c *.y | sort -n | diff - ./output
cd ..
