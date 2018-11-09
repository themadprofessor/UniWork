#!/usr/bin/env bash
cd dijkstra
javac Main.java
for f in ../*.txt; do
	basename $f >> out.txt
	java Main $f >> out.txt
	echo >> out.txt
done

cd ../backtrack
javac Main.java
for f in ../*.txt; do
	basename $f >> out.txt
	java Main $f >> out.txt
	echo >> out.txt
done

cd ..
