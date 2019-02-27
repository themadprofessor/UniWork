#!/usr/bin/env bash
makefile=${1:-Makefile}
threads=("1" "2" "4" "8" "16" "128")
sizes=("32" "64" "1024" "65536" "100000")

for thread in ${threads[@]}; do
    for size in ${sizes[@]}; do
        sed -i "s/-DNTH=[0-9]*/-DNTH=$thread/" ${makefile}
        sed -i "s/-DBUF_SZ=[0-9]*/-DBUF_SZ=$size/" ${makefile}
        make -f ${makefile} -B > /dev/null
        echo -n "Testing $thread threads and $size size: "
        output=`./testbench`
        if [[ $(echo ${output} | sed 's/SUCCESS!/SUCCESS!\n/g' | grep -c SUCCESS -) != ${thread} ]]; then
            echo ${output} | sed 's/^(SUCCESS|FAILURE)! /!\n/g' > "$thread-$size.out"
            echo "Failed, dumping output to $thread-$size.out"
        else
            echo "Success"
        fi
    done
done