CHISEL_VERSION ?= 3.6.0

TOP = TopMain
SIM_TOP = SimTop
FPGATOP = NutShellFPGATop

BUILD_DIR = $(abspath ./build)
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

USE_READY_TO_RUN_NEMU = true

SIMTOP = top.TopMain
IMAGE ?= ready-to-run/linux.bin

DATAWIDTH ?= 64
BOARD ?= sim  # sim  pynq  axu3cg
CORE  ?= inorder  # inorder  ooo  embedded

MILL_ARGS = -td $(@D) BOARD=$(BOARD) CORE=$(CORE)
FPGA_ARGS =

ifeq ($(MFC), 1)
CHISEL_VERSION = 6.0.0-M3
endif

ifneq (,$(filter 3%,$(CHISEL_VERSION)))
MILL_ARGS += --output-file $(@F)
FPGA_ARGS += --infer-rw $(FPGATOP) --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf
else ifneq ($(FIRTOOL),)
MILL_ARGS += --firtool-binary-path $(FIRTOOL)
endif

.DEFAULT_GOAL = verilog

help:
	mill -i generator[$(CHISEL_VERSION)].runMain top.$(TOP) --help $(MILL_ARGS)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i generator[$(CHISEL_VERSION)].runMain top.$(TOP) $(MILL_ARGS) $(FPGA_ARGS)
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
	mill -i generator[$(CHISEL_VERSION)].test.runMain $(SIMTOP) $(MILL_ARGS)
	@sed -i 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)

sim-verilog: $(SIM_TOP_V)
ifneq ($(CHISEL_VERSION), 3.6.0)
	cd $(BUILD_DIR) && bash ../scripts/extract_files.sh $(SIM_TOP_V)
endif

emu: sim-verilog
	$(MAKE) -C ./difftest emu WITH_CHISELDB=0 WITH_CONSTANTIN=0

emu-run: sim-verilog
	$(MAKE) -C ./difftest emu-run

simv: sim-verilog
	$(MAKE) -C ./difftest simv WITH_CHISELDB=0 WITH_CONSTANTIN=0

init:
	git submodule update --init

clean:
	rm -rf $(BUILD_DIR)

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

.PHONY: verilog emu clean help $(REF_SO)
