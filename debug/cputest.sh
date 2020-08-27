#!/bin/bash

TEST_HOME=$AM_HOME/tests/cputest

for test in $(ls $TEST_HOME/tests)
do
    t=${test%.c}
    echo -n -e "\x1b[0m $t: "
    make -C $TEST_HOME ARCH=riscv64-nutshell LOG_BEGIN=0 LOG_END=0 ALL=$t run 2>&1 | grep "HIT GOOD TRAP"
    if [[ $? == 1 ]];
    then
        echo -e "\x1b[31mfail"
    fi
done
