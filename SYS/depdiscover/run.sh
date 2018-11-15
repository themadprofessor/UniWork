cd test
../cmake-build-debug/depdiscover *.l *.c *.y | sort -n | diff - ./output
cd ..
