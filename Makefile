TOP = TopMain
SIM_TOP = SimTop
FPGATOP = Top
REAL_TOP = $(if $(strip $(subst sim,,$(BOARD))),$(FPGATOP),$(SIM_TOP))

BUILD_DIR = $(abspath ./build)

RTL_DIR = $(BUILD_DIR)/rtl
RTL_SUFFIX ?= sv
SIM_TOP_V = $(RTL_DIR)/$(REAL_TOP).$(RTL_SUFFIX) # if use FPGA, use FPGATOP
TOP_V = $(RTL_DIR)/$(TOP).$(RTL_SUFFIX)

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

USE_READY_TO_RUN_NEMU = true

SIMTOP = top.TopMain
IMAGE ?= ready-to-run/linux.bin

DATAWIDTH ?= 64
BOARD ?= sim  # sim  pynq  axu3cg
CORE  ?= inorder  # inorder  ooo  embedded

MILL_ARGS_ALL  = $(MILL_ARGS)
MILL_ARGS_ALL += --target-dir $(RTL_DIR) BOARD=$(BOARD) CORE=$(CORE)
FPGA_ARGS =

ifneq ($(FIRTOOL),)
MILL_ARGS_ALL += --firtool-binary-path $(FIRTOOL)
endif

MILL_ARGS_ALL += --split-verilog

.DEFAULT_GOAL = verilog

help:
	mill -i generator.test.runMain top.$(TOP) --help $(MILL_ARGS_ALL)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i generator.test.runMain top.$(TOP) $(MILL_ARGS_ALL) $(FPGA_ARGS)
	@mv $(SIM_TOP_V) $(TOP_V)
	@for file in $(RTL_DIR)/*.$(RTL_SUFFIX); do                               \
		sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' "$$file";        \
	done
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

deploy: build/top.zip


build/top.zip: $(TOP_V)
	@zip -r $@ $< $<.conf build/*.anno.json

.PHONY: deploy build/top.zip

verilog: $(TOP_V)

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	mill -i generator.test.runMain $(SIMTOP) $(MILL_ARGS_ALL)
	@for file in $(RTL_DIR)/*.$(RTL_SUFFIX); do                               \
		sed -i -e 's/$$fatal/xs_assert_v2(`__FILE__, `__LINE__)/g' "$$file";  \
		sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" "$$file";         \
	done

sim-verilog: $(SIM_TOP_V)

emu: sim-verilog
	$(MAKE) -C ./difftest emu WITH_CHISELDB=0 WITH_CONSTANTIN=0 RTL_SUFFIX=$(RTL_SUFFIX)

emu-run: sim-verilog
	$(MAKE) -C ./difftest emu-run RTL_SUFFIX=$(RTL_SUFFIX)

simv: sim-verilog
	$(MAKE) -C ./difftest simv WITH_CHISELDB=0 WITH_CONSTANTIN=0 RTL_SUFFIX=$(RTL_SUFFIX)

init:
	git submodule update --init

clean:
	rm -rf $(BUILD_DIR)

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

.PHONY: verilog emu clean help $(REF_SO)
