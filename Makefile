CHISEL_VERSION ?= 3.6.0

TOP = TopMain
SIM_TOP = SimTop
FPGATOP = NutShellFPGATop

BUILD_DIR = $(abspath ./build)

RTL_DIR = $(BUILD_DIR)/rtl
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).v
TOP_V = $(RTL_DIR)/$(TOP).v

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

USE_READY_TO_RUN_NEMU = true

SIMTOP = top.TopMain
IMAGE ?= ready-to-run/linux.bin

DATAWIDTH ?= 64
BOARD ?= sim  # sim  pynq  axu3cg
CORE  ?= inorder  # inorder  ooo  embedded

MILL_ARGS_ALL = $(MILL_ARGS)
MILL_ARGS_ALL += -td $(RTL_DIR) BOARD=$(BOARD) CORE=$(CORE)
ifeq ($(HIGH), 1)
MILL_ARGS_ALL += -X high
endif
FPGA_ARGS =

ifeq ($(MFC), 1)
CHISEL_VERSION = 6.0.0-M3
endif

SPLIT_VERILOG = 0

ifneq (,$(filter 3%,$(CHISEL_VERSION)))
MILL_ARGS_ALL += --output-file $(@F)
FPGA_ARGS += --infer-rw $(FPGATOP) --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf
else
SPLIT_VERILOG = 1
endif

ifneq ($(FIRTOOL),)
MILL_ARGS_ALL += --firtool-binary-path $(FIRTOOL)
endif

EXTRACTOR = $(abspath ./scripts/extract_files.sh)

.DEFAULT_GOAL = verilog

BUILD_MODEL = $(BUILD_DIR)/rtl/SimTop

gen:
	circt-translate --import-firrtl $(BUILD_MODEL).fir > $(BUILD_MODEL).fir.mlir
	circt-opt -pass-pipeline='builtin.module(firrtl.circuit(firrtl.module(firrtl-lower-chirrtl)))' $(BUILD_MODEL).fir.mlir > $(BUILD_MODEL).fir.opt.mlir
	circt-translate --export-firrtl $(BUILD_MODEL).fir.opt.mlir > $(BUILD_MODEL).hi.fir

help:
	mill -i generator[$(CHISEL_VERSION)].runMain top.$(TOP) --help $(MILL_ARGS_ALL)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i generator[$(CHISEL_VERSION)].runMain top.$(TOP) $(MILL_ARGS_ALL) $(FPGA_ARGS)
ifeq ($(SPLIT_VERILOG), 1)
	@mv $(SIM_TOP_V) $(TOP_V)
	@cd $(RTL_DIR) && bash $(EXTRACTOR) $(TOP_V)
endif
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
	mill -i generator[$(CHISEL_VERSION)].test.runMain $(SIMTOP) $(MILL_ARGS_ALL)
	@sed -i 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)
ifeq ($(MFC), 1)
	@sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" $(SIM_TOP_V)
	@cd $(RTL_DIR) && bash $(EXTRACTOR) $(SIM_TOP_V)
endif

sim-verilog: $(SIM_TOP_V)

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
