#include "common.h"
#include "difftest.h"
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifndef REF_SO
# error Please define REF_SO to the path of NEMU shared object file
#endif

void (*ref_difftest_memcpy_from_dut)(paddr_t dest, void *src, size_t n) = NULL;
void (*ref_difftest_getregs)(void *c) = NULL;
void (*ref_difftest_setregs)(const void *c) = NULL;
void (*ref_difftest_exec)(uint64_t n) = NULL;
void (*ref_difftest_raise_intr)(uint64_t NO) = NULL;
void (*ref_isa_reg_display)(void) = NULL;

static bool is_skip_ref;
static bool is_skip_dut;

// this is used to let ref skip instructions which
// can not produce consistent behavior with NEMU
void difftest_skip_ref() {
  is_skip_ref = true;
}

// this is used to deal with instruction packing in QEMU.
// Sometimes letting QEMU step once will execute multiple instructions.
// We should skip checking until NEMU's pc catches up with QEMU's pc.
void difftest_skip_dut() {
  if (is_skip_dut) return;

  ref_difftest_exec(1);
  is_skip_dut = true;
}

void init_difftest(rtlreg_t *reg) {
  void *handle;
  handle = dlopen(REF_SO, RTLD_LAZY | RTLD_DEEPBIND);
  assert(handle);

  ref_difftest_memcpy_from_dut = (void (*)(paddr_t, void *, size_t))dlsym(handle, "difftest_memcpy_from_dut");
  assert(ref_difftest_memcpy_from_dut);

  ref_difftest_getregs = (void (*)(void *))dlsym(handle, "difftest_getregs");
  assert(ref_difftest_getregs);

  ref_difftest_setregs = (void (*)(const void *))dlsym(handle, "difftest_setregs");
  assert(ref_difftest_setregs);

  ref_difftest_exec = (void (*)(uint64_t))dlsym(handle, "difftest_exec");
  assert(ref_difftest_exec);

  ref_difftest_raise_intr = (void (*)(uint64_t))dlsym(handle, "difftest_raise_intr");
  assert(ref_difftest_raise_intr);

  ref_isa_reg_display = (void (*)(void))dlsym(handle, "isa_reg_display");
  assert(ref_isa_reg_display);

  void (*ref_difftest_init)(void) = (void (*)(void))dlsym(handle, "difftest_init");
  assert(ref_difftest_init);

  ref_difftest_init();
  void* get_img_start();
  long get_img_size();
  ref_difftest_memcpy_from_dut(0x80000000, get_img_start(), get_img_size());
  ref_difftest_setregs(reg);
}

static const char *reg_name[DIFFTEST_NR_REG] = {
  "$0", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6",
  "this_pc"
#ifndef __RV32__
  ,"mstatus", "mcause", "mepc",
  "sstatus", "scause", "sepc"
#endif
  ,"v[0][0]", "v[0][1]", "v[0][2]", "v[0][3]", "v[1][0]", "v[1][1]", "v[1][2]", "v[1][3]",
  "v[2][0]", "v[2][1]", "v[2][2]", "v[2][3]", "v[3][0]", "v[3][1]", "v[3][2]", "v[3][3]",
  "v[4][0]", "v[4][1]", "v[4][2]", "v[4][3]", "v[5][0]", "v[5][1]", "v[5][2]", "v[5][3]",
  "v[6][0]", "v[6][1]", "v[6][2]", "v[6][3]", "v[7][0]", "v[7][1]", "v[7][2]", "v[7][3]",
  "v[8][0]", "v[8][1]", "v[8][2]", "v[8][3]", "v[9][0]", "v[9][1]", "v[9][2]", "v[9][3]",
  "v[10][0]", "v[10][1]", "v[10][2]", "v[10][3]", "v[11][0]", "v[11][1]", "v[11][2]", "v[11][3]",
  "v[12][0]", "v[12][1]", "v[12][2]", "v[12][3]", "v[13][0]", "v[13][1]", "v[13][2]", "v[13][3]",
  "v[14][0]", "v[14][1]", "v[14][2]", "v[14][3]", "v[15][0]", "v[15][1]", "v[15][2]", "v[15][3]",
  "v[16][0]", "v[16][1]", "v[16][2]", "v[16][3]", "v[17][0]", "v[17][1]", "v[17][2]", "v[17][3]",
  "v[18][0]", "v[18][1]", "v[18][2]", "v[18][3]", "v[19][0]", "v[19][1]", "v[19][2]", "v[19][3]",
  "v[20][0]", "v[20][1]", "v[20][2]", "v[20][3]", "v[21][0]", "v[21][1]", "v[21][2]", "v[21][3]",
  "v[22][0]", "v[22][1]", "v[22][2]", "v[22][3]", "v[23][0]", "v[23][1]", "v[23][2]", "v[23][3]",
  "v[24][0]", "v[24][1]", "v[24][2]", "v[24][3]", "v[25][0]", "v[25][1]", "v[25][2]", "v[25][3]",
  "v[26][0]", "v[26][1]", "v[26][2]", "v[26][3]", "v[27][0]", "v[27][1]", "v[27][2]", "v[27][3]",
  "v[28][0]", "v[28][1]", "v[28][2]", "v[28][3]", "v[29][0]", "v[29][1]", "v[29][2]", "v[29][3]",
  "v[30][0]", "v[30][1]", "v[30][2]", "v[30][3]", "v[31][0]", "v[31][1]", "v[31][2]", "v[31][3]",
  "vstart", "vxsat", "vxrm", "vl", "vtype",
};

int difftest_step(rtlreg_t *reg_scala, uint32_t this_inst,
  int isMMIO, int isRVC, int isRVC2, uint64_t intrNO, int priviledgeMode, 
  int isMultiCommit
  ) {

  // Note:
  // reg_scala[DIFFTEST_THIS_PC] is the first PC commited by CPU-WB
  // ref_r[DIFFTEST_THIS_PC] is NEMU's next PC
  // To skip the compare of an instruction, replace NEMU reg value with CPU's regfile value,
  // then set NEMU's PC to next PC to be run

  #define DEBUG_RETIRE_TRACE_SIZE 16

  rtlreg_t ref_r[DIFFTEST_NR_REG];
  rtlreg_t this_pc = reg_scala[DIFFTEST_THIS_PC];
  // ref_difftest_getregs() will get the next pc,
  // therefore we must keep track this one
  static rtlreg_t nemu_this_pc = 0x80000000;
  static rtlreg_t pc_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static uint32_t inst_retire_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static uint32_t multi_commit_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static uint32_t skip_queue[DEBUG_RETIRE_TRACE_SIZE] = {0};
  static int pc_retire_pointer = 7;
  static int need_copy_pc = 0;
  #ifdef NO_DIFFTEST
  return 0;
  #endif

  if (need_copy_pc) {
    need_copy_pc = 0;
    ref_difftest_getregs(&ref_r); // get rf info saved in former cycle
    nemu_this_pc = reg_scala[DIFFTEST_THIS_PC]; // we assume target's pc is right
    ref_r[DIFFTEST_THIS_PC] = reg_scala[DIFFTEST_THIS_PC];
    ref_difftest_setregs(ref_r);
  }

  if (isMMIO) {
    // printf("diff pc: %x isRVC %x\n", this_pc, isRVC);
    // MMIO accessing should not be a branch or jump, just +2/+4 to get the next pc
    int pc_skip = 0;
    pc_skip += isRVC ? 2 : 4;
    pc_skip += isMultiCommit ? (isRVC2 ? 2 : 4) : 0;
    reg_scala[DIFFTEST_THIS_PC] += pc_skip;
    nemu_this_pc += pc_skip;
    // to skip the checking of an instruction, just copy the reg state to reference design
    ref_difftest_setregs(reg_scala);
    pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
    pc_retire_queue[pc_retire_pointer] = this_pc;
    inst_retire_queue[pc_retire_pointer] = this_inst;
    multi_commit_queue[pc_retire_pointer] = isMultiCommit;
    skip_queue[pc_retire_pointer] = isMMIO;
    need_copy_pc = 1;
    return 0;
  }


  if (intrNO) {
    ref_difftest_raise_intr(intrNO);
  } else {
    ref_difftest_exec(1);
  }

  if (isMultiCommit) {    
    ref_difftest_exec(1);
    // exec 1 more cycle
  }

  ref_difftest_getregs(&ref_r);

  rtlreg_t next_pc = ref_r[32];
  pc_retire_pointer = (pc_retire_pointer+1) % DEBUG_RETIRE_TRACE_SIZE;
  pc_retire_queue[pc_retire_pointer] = this_pc;
  inst_retire_queue[pc_retire_pointer] = this_inst;
  multi_commit_queue[pc_retire_pointer] = isMultiCommit;
  skip_queue[pc_retire_pointer] = isMMIO;

  int isCSR = ((this_inst & 0x7f) ==  0x73);
  int isCSRMip = ((this_inst >> 20) == 0x344) && isCSR;
  if (isCSRMip) {
    // We can not handle NEMU.mip.mtip since it is driven by CLINT,
    // which is not accessed in NEMU due to MMIO.
    // Just sync the state of NEMU from NutCore.
    reg_scala[DIFFTEST_THIS_PC] = next_pc;
    nemu_this_pc = next_pc;
    ref_difftest_setregs(reg_scala);
    return 0;
  }

  // replace with "this pc" for checking
  ref_r[DIFFTEST_THIS_PC] = nemu_this_pc;
  nemu_this_pc = next_pc;

  ref_r[0] = 0;

  if (memcmp(reg_scala, ref_r, sizeof(ref_r)) != 0) {
    printf("\n==============Retire Trace==============\n");
    int j;
    for(j = 0; j < DEBUG_RETIRE_TRACE_SIZE; j++){
      printf("retire trace [%x]: pc %010lx inst %08x %s %s %s\n", j, 
        pc_retire_queue[j], 
        inst_retire_queue[j], 
        (multi_commit_queue[j])?"MC":"  ", 
        (skip_queue[j])?"SKIP":"    ", 
        (j==pc_retire_pointer)?"<--":""
      );
    }
    printf("\n==============  Reg Diff  ==============\n");
    ref_isa_reg_display();
    printf("priviledgeMode = %d\n", priviledgeMode);
    puts("");
    int i;
    for (i = 0; i < DIFFTEST_NR_REG; i ++) {
      if (reg_scala[i] != ref_r[i]) {
        printf("%s different at pc = 0x%010lx, right= 0x%016lx, wrong = 0x%016lx\n",
            reg_name[i], this_pc, ref_r[i], reg_scala[i]);
      }
    }
    return 1;
  }
  return 0;
}
