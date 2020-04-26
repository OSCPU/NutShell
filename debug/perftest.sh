echo run all benchmarks:
echo median
make rvtest RVTEST_TARGET=median 2>&1 | tee > median.log
echo multiply
make rvtest RVTEST_TARGET=multiply 2>&1 | tee > multiply.log
echo qsort
make rvtest RVTEST_TARGET=qsort 2>&1 | tee > qsort.log
echo rsort
make rvtest RVTEST_TARGET=rsort 2>&1 | tee > rsort.log
echo towers
make rvtest RVTEST_TARGET=towers 2>&1 | tee > towers.log
echo vvadd
make rvtest RVTEST_TARGET=vvadd 2>&1 | tee > vvadd.log
echo smoke-test
make smoke-test
echo coremark
make coremark
echo dhrystone
make dhrystone
echo microbench-train
make microbench-train