include Makefile.check

default: project

STANDALONE ?= false

# vivado project name
PRJ ?= myproject
PRJ_FULL = $(PRJ)-$(BOARD)
VIVADO_FLAG = -nolog -nojournal -notrace

#--------------------------------------------------------------------
# Project building and implementation
#--------------------------------------------------------------------

PRJ_ROOT = board/$(BOARD)/build/$(PRJ_FULL)
XPR_FILE = $(PRJ_ROOT)/$(PRJ_FULL).xpr
$(XPR_FILE):
	make -C .. BOARD=$(BOARD)
	vivado $(VIVADO_FLAG) -mode batch -source board/$(BOARD)/mk.tcl -tclargs $(PRJ_FULL) $(STANDALONE)

project: $(XPR_FILE)

bootgen:
	cd boot && hsi $(VIVADO_FLAG) -source mk.tcl -tclargs $(PRJ_FULL) $(STANDALONE)

vivado: $(XPR_FILE)
	vivado $(VIVADO_FLAG) $(XPR_FILE) &

.PHONY: default project vivado bootgen
