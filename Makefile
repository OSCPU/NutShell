TOP = TopMain
SIM_TOP = SimTop
FPGATOP = NutShellFPGATop

BUILD_DIR = ./build
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).sv
TOP_V = $(BUILD_DIR)/$(TOP).sv
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

USE_READY_TO_RUN_NEMU = true

SIMTOP = top.TopMain
IMAGE ?= ready-to-run/linux.bin

DATAWIDTH ?= 64
BOARD ?= sim  # sim  pynq  axu3cg
CORE  ?= inorder  # inorder  ooo  embedded

.DEFAULT_GOAL = verilog

help:
	mill -i chiselModule.runMain top.$(TOP) --help BOARD=$(BOARD) CORE=$(CORE)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i chiselModule.runMain top.$(TOP) -td $(@D) --split-verilog BOARD=$(BOARD) CORE=$(CORE)
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
	mill -i chiselModule.test.runMain $(SIMTOP) -td $(@D) --split-verilog BOARD=sim CORE=$(CORE)
	@find . -type f -name "*.sv" -print0 | xargs -0 sed -i 's/$$fatal/xs_assert(`__LINE__)/g'
	@find . -type f -name "*.sv" -print0 | xargs -0 sed -i 's/$$error(/$$fwrite(32'\''h80000002, /g'

sim-verilog: $(SIM_TOP_V)

emu: sim-verilog
	$(MAKE) -C ./difftest emu WITH_CHISELDB=0 WITH_CONSTANTIN=0

emu-run: sim-verilog
	$(MAKE) -C ./difftest emu-run

init:
	git submodule update --init

clean:
	rm -rf $(BUILD_DIR)

bsp:
	mill -i mill.bsp.BSP/install

mill:
	mill -i mill.idea.GenIdea/idea

.PHONY: verilog emu clean help $(REF_SO)
