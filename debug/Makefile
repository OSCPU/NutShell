# ------------------------------------------------------------------
# test settings
# ------------------------------------------------------------------

LOG_BEGIN ?= 0
LOG_END ?= 0

ARCH = ARCH=riscv64-nutshell
# ------------------------------------------------------------------

SINGLETEST = recursion
RVTEST_TARGET = towers
NANOS_HOME ?= $(AM_HOME)/../nanos-lite
EMU_ARGS = B=$(LOG_BEGIN) E=$(LOG_END)
CPU_TEST_RESULT = $(shell cat cpu.log | grep -E "different|stuck")
MB_RESULT = $(shell cat microbench.log | grep IPC)
MBT_RESULT = $(shell cat microbench-train.log | grep IPC)
CM_RESULT = $(shell cat coremark.log | grep IPC)

# ------------------------------------------------------------------
# bulid CPU and run dummy test
# ------------------------------------------------------------------

cpu:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) $(EMU_ARGS) ALL=dummy run

# ------------------------------------------------------------------
# unit tests
# ------------------------------------------------------------------

cputest:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) $(EMU_ARGS) run 2>&1 | tee > cpu.log
	# cat cpu.log | grep different
ifeq ($(CPU_TEST_RESULT),)
	@echo "simple CPU test passed"
else
	@echo $(CPU_TEST_RESULT)
	@echo "simple CPU test failed"
endif

rvtest-ui:
	$(MAKE) -C $(RVTEST_HOME)/isa noop_run LOG_LEVEL=OFF SUITES=rv64ui ENV=p

rvtest:
	$(MAKE) -C $(RVTEST_HOME)/isa noop_run LOG_LEVEL=OFF SUITES=rv64ui ENV=p
	$(MAKE) -C $(RVTEST_HOME)/isa noop_run LOG_LEVEL=OFF SUITES=rv64um ENV=p
	$(MAKE) -C $(RVTEST_HOME)/isa noop_run LOG_LEVEL=OFF SUITES=rv64uc ENV=p
	$(MAKE) -C $(RVTEST_HOME)/isa noop_run LOG_LEVEL=OFF SUITES=rv64si ENV=p
	$(MAKE) -C $(RVTEST_HOME)/isa noop_run LOG_LEVEL=OFF SUITES=rv64mi ENV=p

bputest:
	$(MAKE) -C $(AM_HOME)/tests/bputest $(ARCH) $(EMU_ARGS) ALL=forcall2 run 2>&1 | tee > bpu.log
	cat bpu.log | grep Mbp

unittest:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) $(EMU_ARGS) ALL=$(SINGLETEST) run 2>&1 | tee > test.log
	cat test.log | grep -E "different|stuck|GOOD"
	cat test.log | grep ISU > isu.log

microbench:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) $(EMU_ARGS) mainargs=test run 2>&1 | tee > microbench.log
	cat microbench.log | grep IPC
	@date >> history.log
	@git log --pretty=format:"%h %an %ad %s" -1 >> history.log
	@echo -e "" >> history.log
	@echo $(MB_RESULT) >> history.log
	@echo -e "" >> history.log

microbench-train:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) $(EMU_ARGS) mainargs=train run 2>&1 | tee > microbench-train.log
	cat microbench-train.log | grep IPC

coremark:
	$(MAKE) -C $(AM_HOME)/apps/coremark $(ARCH) $(EMU_ARGS) mainargs=test run 2>&1 | tee > coremark.log
	cat coremark.log | grep IPC
	@date >> cm_history.log
	@git log --pretty=format:"%h %an %ad %s" -1 >> cm_history.log
	@echo -e "" >> cm_history.log
	@echo $(CM_RESULT) >> cm_history.log
	@echo -e "" >> cm_history.log


dhrystone:
	$(MAKE) -C $(AM_HOME)/apps/dhrystone $(ARCH) $(EMU_ARGS) mainargs=test run 2>&1 | tee > dhrystone.log
	cat dhrystone.log | grep IPC

xj:
	$(MAKE) -C $(NANOS_HOME) $(ARCH) $(EMU_ARGS) run

xjnemu:
	$(MAKE) -C $(NANOS_HOME) ARCH=riscv64-nemu run

rttos:
	$(MAKE) -C $(RTTOS_HOME)/bsp/riscv64-nutshell run

rttos-debug:
	$(MAKE) -C $(RTTOS_HOME)/bsp/riscv64-nutshell run 2>&1 | tee > rttos.log

freertos:
	$(MAKE) -C $(FREERTOS_HOME)/Demo/riscv64-nutshell nutshell_run

xv6:
	$(MAKE) -C $(XV6_HOME) nutshell

xv6-debug:
	$(MAKE) -C $(XV6_HOME) nutshell 2>&1 | tee > xv6.log

linux:
	$(MAKE) -C $(BBL_LINUX_HOME) nutshell

linux-debug:
	$(MAKE) -C $(BBL_LINUX_HOME) nutshell 2>&1 | tee > linux.log

# ------------------------------------------------------------------
# test set
# ------------------------------------------------------------------

smoke-test: cpu cputest microbench coremark
	@echo "Smoke Test Passed"

regression-test: smoke-test coremark linux
	@echo "Regression Test Passed"

perf-test: smoke-test microbench-train
	@echo "Perf Test Finished"

# ------------------------------------------------------------------
# get disassembled test src
# ------------------------------------------------------------------

disassemble-unittest:
	cp $(AM_HOME)/tests/cputest/build/$(SINGLETEST)-riscv64-noop.txt ./d-unittest.log

disassemble-rttos:
	cp $(RTTOS_HOME)/bsp/riscv64-nutshell/build/code.txt ./d-rttos.log

disassemble-freertos:
	cp $(FREERTOS_HOME)/Demo/riscv64-nutshell/build/FreeRTOS-simple.elf.txt ./d-freertos.log

disassemble-xv6:
	cp $(XV6_HOME)/build/code.txt ./d-xv6.log

# ------------------------------------------------------------------
# chore
# ------------------------------------------------------------------

clean:
	$(MAKE) -C .. clean