TOP = TopMain
FPGATOP = NutShellFPGATop
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
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
	mill chiselModule.runMain top.$(TOP) --help BOARD=$(BOARD) CORE=$(CORE)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill chiselModule.runMain top.$(TOP) -td $(@D) --output-file $(@F) --infer-rw $(FPGATOP) --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf BOARD=$(BOARD) CORE=$(CORE)
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

SIM_TOP = SimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	mill chiselModule.test.runMain $(SIMTOP) -td $(@D) --output-file $(@F) BOARD=sim CORE=$(CORE)

sim-verilog: $(SIM_TOP_V)

emu: sim-verilog
	$(MAKE) -C ./difftest emu

emu-run: sim-verilog
	$(MAKE) -C ./difftest emu-run

init:
	git submodule update --init

clean:
	rm -rf $(BUILD_DIR)

bsp:
	mill -i mill.bsp.BSP/install

.PHONY: verilog emu clean help $(REF_SO)
