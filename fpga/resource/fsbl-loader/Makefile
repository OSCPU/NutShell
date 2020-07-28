RM := rm -rf

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(strip $(S_UPPER_DEPS)),)
-include $(S_UPPER_DEPS)
endif
ifneq ($(strip $(C_DEPS)),)
-include $(C_DEPS)
endif
endif

APP = fsbl-loader
OBJS += $(APP).o
LIBS := -Wl,--start-group,-lxilffs,-lxil,-lgcc,-lc,--end-group
LDSCRIPT = lscript.ld
FSBL_LOC = ../../boot/build/argo-pynq/fsbl
BSP_PATH = $(FSBL_LOC)/zynq_fsbl_bsp/ps7_cortexa9_0
INCLUDEPATH := -I$(BSP_PATH)/include -I.
LIBPATH := -L$(BSP_PATH)/lib

CC = arm-none-eabi-gcc
CFLAGS = -MMD -MP -mcpu=cortex-a9 -mfpu=vfpv3 -mfloat-abi=hard -Wall -O2 $(INCLUDEPATH)
LN_FLAGS = -mcpu=cortex-a9 -mfpu=vfpv3 -mfloat-abi=hard -Wl,-build-id=none -specs=$(FSBL_LOC)/Xilinx.spec  -lrsa -Wl,--start-group,-lxil,-lgcc,-lc,--end-group -Wl,--start-group,-lxilffs,-lxil,-lgcc,-lc,--end-group -Wl,--start-group,-lrsa,-lxil,-lgcc,-lc,--end-group  -Wl,--gc-sections


all: $(APP).elf

$(APP).elf: $(OBJS) $(LDSCRIPT) $(USER_OBJS)
	$(CC) $(LN_FLAGS) -Wl,-T -Wl,$(LDSCRIPT) $(LIBPATH) -o $@ $(OBJS) $(USER_OBJS) $(LIBS)

# Other Targets
clean:
	-$(RM) $(APP).elf *.d *.o
	-@echo ' '

%.o: %.c
	$(CC) $(CFLAGS) -c -o "$@" "$<"

.PHONY: all clean dependents
