#!/usr/bin/env bash
# Set the makefile to be used to either the first argument or Makefile
makefile=${1:-Makefile}
# Specify an array of thread counts to tests with
threads=("1" "2" "4" "8" "16" "128")
# Specify an array of data sizes to tests with
sizes=("32" "64" "1024" "65536" "100000")

# Iterate over the above arrays
for thread in ${threads[@]}; do
    for size in ${sizes[@]}; do
        # Modify the make file to change the thread count and data sizes for the testbench
        sed -i "s/-DNTH=[0-9]*/-DNTH=$thread/" ${makefile}
        sed -i "s/-DBUF_SZ=[0-9]*/-DBUF_SZ=$size/" ${makefile}
        # Compile the testbench with the makefile, ignoring all make output
        make -f ${makefile} -B > /dev/null
        echo -n "Testing $thread threads and $size size: "
        # Run the testbench and store its output
        output=`./testbench`

        # When storing the output of the testbench, the newlines are removed, so the sed below adds them back
        # If the number of threads which don't succeed doesn't equal the thread count, dump the output to a file
        # Otherwise continue with the tests
        if [[ $(echo ${output} | sed 's/SUCCESS!/SUCCESS!\n/g' | grep -c SUCCESS -) != ${thread} ]]; then
            echo ${output} | sed 's/! /!\n/g' > "$thread-$size.out"
            echo "Failed, dumping output to $thread-$size.out"
        else
            echo "Success"
        fi
    done
done