TOP = TopMain
SIM_TOP = SimTop
FPGATOP = NutShellFPGATop

BUILD_DIR = $(abspath ./build)

RTL_DIR = $(BUILD_DIR)/rtl
RTL_SUFFIX ?= sv
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).$(RTL_SUFFIX)
TOP_V = $(RTL_DIR)/$(TOP).$(RTL_SUFFIX)

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

USE_READY_TO_RUN_NEMU = true

SIMTOP = top.TopMain
IMAGE ?= ready-to-run/linux.bin

DATAWIDTH ?= 64
BOARD ?= sim  # sim  pynq  axu3cg
CORE  ?= inorder  # inorder  ooo  embedded

MILL_ARGS = -td $(RTL_DIR) BOARD=$(BOARD) CORE=$(CORE)
FPGA_ARGS =

# If firtool is not specified and not found in PATH, download and cache it.
ifeq ($(FIRTOOL),)
ifeq ($(shell which firtool 2>/dev/null),)
FIRTOOL_VERSION = 1.61.0
FIRTOOL_DIR = $(HOME)/.cache/firtool
FIRTOOL_BIN = $(FIRTOOL_DIR)/firtool-$(FIRTOOL_VERSION)/bin/firtool
ifeq ($(wildcard $(HOME)/.cache/firtool/firtool-$(FIRTOOL_VERSION)/bin),)
$(info [INFO] Firtool not found.)
FIRTOOL_URL = https://github.com/llvm/circt/releases/download/firtool-$(FIRTOOL_VERSION)/firrtl-bin-linux-x64.tar.gz
$(info [INFO] Downloading from $(FIRTOOL_URL) to $(FIRTOOL_BIN))
$(shell mkdir -p $(FIRTOOL_DIR) && curl -L $(FIRTOOL_URL) | tar -xzC $(FIRTOOL_DIR))
endif
FIRTOOL = $(FIRTOOL_BIN)
endif
endif

ifneq ($(FIRTOOL),)
MILL_ARGS += --firtool-binary-path $(FIRTOOL)
endif

MILL_ARGS += --split-verilog

.DEFAULT_GOAL = verilog

help:
	mill -i generator.test.runMain top.$(TOP) --help $(MILL_ARGS)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i generator.test.runMain top.$(TOP) $(MILL_ARGS) $(FPGA_ARGS)
	@mv $(SIM_TOP_V) $(TOP_V)
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
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
	mill -i generator.test.runMain $(SIMTOP) $(MILL_ARGS)
	@sed -i 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)
	@sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" $(SIM_TOP_V)

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
