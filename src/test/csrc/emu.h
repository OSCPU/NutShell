#include <cstdlib>
#include <cassert>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include "difftest.h"

//#include "VSimTop__Dpi.h"
#include "common.h"
// #include "VNutShellSimTop.h"
#include "VysyxSoCFull.h"
#include "VysyxSoCFull___024root.h"
#if VM_TRACE
#include <verilated_vcd_c.h>	// Trace file format header
#endif

extern "C" void flash_init(char *img);
extern "C" void flash_memcpy(void *src, int len);

class Emulator {
  const char *image;
  std::shared_ptr<VysyxSoCFull> dut_ptr;
  // std::shared_ptr<VNutShellSimTop> dut_ptr;
#if VM_TRACE
  VerilatedVcdC* tfp;
#endif

  // emu control variable
  uint32_t seed;
  uint64_t max_cycles, cycles;
  uint64_t log_begin, log_end, log_level;

  std::vector<const char *> parse_args(int argc, const char *argv[]);

  static const struct option long_options[];
  static void print_help(const char *file);

/*
  void read_emu_regs(rtlreg_t *r) {
#define macro(x) r[x] = dut_ptr->io_difftest_r_##x
    macro(0); macro(1); macro(2); macro(3); macro(4); macro(5); macro(6); macro(7);
    macro(8); macro(9); macro(10); macro(11); macro(12); macro(13); macro(14); macro(15);
    macro(16); macro(17); macro(18); macro(19); macro(20); macro(21); macro(22); macro(23);
    macro(24); macro(25); macro(26); macro(27); macro(28); macro(29); macro(30); macro(31);
    r[DIFFTEST_THIS_PC] = dut_ptr->io_difftest_thisPC;
#ifndef __RV32__
    r[DIFFTEST_MSTATUS] = dut_ptr->io_difftest_mstatus;
    r[DIFFTEST_SSTATUS] = dut_ptr->io_difftest_sstatus;
    r[DIFFTEST_MEPC   ] = dut_ptr->io_difftest_mepc;
    r[DIFFTEST_SEPC   ] = dut_ptr->io_difftest_sepc;
    r[DIFFTEST_MCAUSE ] = dut_ptr->io_difftest_mcause;
    r[DIFFTEST_SCAUSE ] = dut_ptr->io_difftest_scause;
#endif
    #define Vmacro(x) r[39 + x] = dut_ptr->io_difftest_vr_##x // TODO: how to simplify
        Vmacro(0);   Vmacro(1);   Vmacro(2);   Vmacro(3);   Vmacro(4);   Vmacro(5);   Vmacro(6);   Vmacro(7);
        Vmacro(8);   Vmacro(9);   Vmacro(10);  Vmacro(11);  Vmacro(12);  Vmacro(13);  Vmacro(14);  Vmacro(15);
        Vmacro(16);  Vmacro(17);  Vmacro(18);  Vmacro(19);  Vmacro(20);  Vmacro(21);  Vmacro(22);  Vmacro(23);
        Vmacro(24);  Vmacro(25);  Vmacro(26);  Vmacro(27);  Vmacro(28);  Vmacro(29);  Vmacro(30);  Vmacro(31);
        Vmacro(32);  Vmacro(33);  Vmacro(34);  Vmacro(35);  Vmacro(36);  Vmacro(37);  Vmacro(38);  Vmacro(39);
        Vmacro(40);  Vmacro(41);  Vmacro(42);  Vmacro(43);  Vmacro(44);  Vmacro(45);  Vmacro(46);  Vmacro(47);
        Vmacro(48);  Vmacro(49);  Vmacro(50);  Vmacro(51);  Vmacro(52);  Vmacro(53);  Vmacro(54);  Vmacro(55);
        Vmacro(56);  Vmacro(57);  Vmacro(58);  Vmacro(59);  Vmacro(60);  Vmacro(61);  Vmacro(62);  Vmacro(63);
        Vmacro(64);  Vmacro(65);  Vmacro(66);  Vmacro(67);  Vmacro(68);  Vmacro(69);  Vmacro(70);  Vmacro(71);
        Vmacro(72);  Vmacro(73);  Vmacro(74);  Vmacro(75);  Vmacro(76);  Vmacro(77);  Vmacro(78);  Vmacro(79);
        Vmacro(80);  Vmacro(81);  Vmacro(82);  Vmacro(83);  Vmacro(84);  Vmacro(85);  Vmacro(86);  Vmacro(87);
        Vmacro(88);  Vmacro(89);  Vmacro(90);  Vmacro(91);  Vmacro(92);  Vmacro(93);  Vmacro(94);  Vmacro(95);
        Vmacro(96);  Vmacro(97);  Vmacro(98);  Vmacro(99);  Vmacro(100); Vmacro(101); Vmacro(102); Vmacro(103);
        Vmacro(104); Vmacro(105); Vmacro(106); Vmacro(107); Vmacro(108); Vmacro(109); Vmacro(110); Vmacro(111);
        Vmacro(112); Vmacro(113); Vmacro(114); Vmacro(115); Vmacro(116); Vmacro(117); Vmacro(118); Vmacro(119);
        Vmacro(120); Vmacro(121); Vmacro(122); Vmacro(123); Vmacro(124); Vmacro(125); Vmacro(126); Vmacro(127);
    r[DIFFTEST_VSTART] = dut_ptr->io_difftest_vstart;
    r[DIFFTEST_VXSAT ] = dut_ptr->io_difftest_vxsat;
    r[DIFFTEST_VXRM  ] = dut_ptr->io_difftest_vxrm;
    r[DIFFTEST_VL    ] = dut_ptr->io_difftest_vl;
    r[DIFFTEST_VTYPE ] = dut_ptr->io_difftest_vtype;
  }
*/

  public:
  // argv decay to the secondary pointer
  Emulator(int argc, const char *argv[]):
    image(nullptr),
    dut_ptr(new std::remove_reference<decltype(*dut_ptr)>::type),
    seed(0), max_cycles(-1), cycles(0),
    log_begin(0), log_end(-1), log_level(LOG_ALL)
  {
    // init emu
    auto args = parse_args(argc, argv);

    // srand
    srand(seed);
    srand48(seed);
    Verilated::randReset(2);
    Verilated::commandArgs(argc, argv);

    // set log time range and log level
    /*
    dut_ptr->io_logCtrl_log_begin = log_begin;
    dut_ptr->io_logCtrl_log_end = log_end;
    dut_ptr->io_logCtrl_log_level = log_level;
    */

    // init ram
    extern void init_ram(const char *img);
    extern void* get_img_start();
    extern long get_img_size();
    init_ram(image);
    memcpy(&dut_ptr->rootp->ysyxSoCFull__DOT__mem__DOT__srams__DOT__mem__DOT__mem_ext__DOT__ram,
        get_img_start(), get_img_size());

    // init flash
    // flash_init("/home53/wkf/ysyx/ysyxSoC/src/main/resources/ysyx-peripheral/bin/hello-flash.bin");
    // flash_init("/home53/wkf/ysyx/hello-loader.bin");
    //flash_init("/home/yzh/oscpu/ysyxSoC/ysyx/program/bin/loader/rtthread-loader.bin");
    uint32_t jump_fw [] = {
      0x0010009b,  // addiw   ra,zero,1
      0x01f09093,  // slli    ra,ra,0x1f
      0x00008067,  // ret
    };
    flash_memcpy(jump_fw, sizeof(jump_fw));
    // flash_init("/home53/wkf/ysyx/ysyxSoC-new/ysyx/program/bin/flash/rtthread-flash.bin");
    // flash_init("/home53/wkf/ysyx/rt-thread/bsp/qemu-riscv-virt64/rtthread.bin");

    // init device
    extern void init_device(void);
    init_device();

    // init core
    reset_ncycles(10);
  }

  void reset_ncycles(size_t cycles) {
    for(int i = 0; i < cycles; i++) {
      dut_ptr->reset = 1;
      dut_ptr->clock = 0;
      dut_ptr->eval();
      dut_ptr->clock = 1;
      dut_ptr->eval();
      dut_ptr->reset = 0;
    }
  }

  void single_cycle() {
    dut_ptr->clock = 0;
    dut_ptr->eval();

#if VM_TRACE
    tfp->dump(cycles);
#endif

    cycles ++;

    dut_ptr->clock = 1;
    dut_ptr->eval();

#if VM_TRACE
    tfp->dump(cycles);
#endif

    cycles ++;

  }

  void execute_cycles(uint64_t n) {
    extern bool is_finish();
    extern void poll_event(void);
    extern uint32_t uptime(void);
    extern void set_abort(void);
    uint32_t lasttime = 0;
    uint64_t lastcommit = n;
    int hascommit = 0;
    const int stuck_limit = 2000;

#if VM_TRACE
    Verilated::traceEverOn(true);	// Verilator must compute traced signals
    VL_PRINTF("Enabling waves...\n");
    tfp = new VerilatedVcdC;
    dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
    tfp->open("vlt_dump.vcd");	// Open the dump file
#endif

    while (!is_finish() && n > 0) {
      single_cycle();
      n --;

      /*
      if (lastcommit - n > stuck_limit && hascommit) {
        eprintf("No instruction commits for %d cycles, maybe get stuck\n"
            "(please also check whether a fence.i instruction requires more than %d cycles to flush the icache)\n",
            stuck_limit, stuck_limit);
#if VM_TRACE
        tfp->close();
#endif
        set_abort();
      }

      if (!hascommit && (uint32_t)dut_ptr->io_difftest_thisPC == 0x80000000) {
        hascommit = 1;
        extern void init_difftest(rtlreg_t *reg);
        rtlreg_t reg[DIFFTEST_NR_REG];
        read_emu_regs(reg);
        init_difftest(reg);
      }

      // difftest
      if (dut_ptr->io_difftest_commit && hascommit) {
        rtlreg_t reg[DIFFTEST_NR_REG];
        read_emu_regs(reg);

        extern int difftest_step(rtlreg_t *reg_scala, uint32_t this_inst,
          int isMMIO, int isRVC, int isRVC2, uint64_t intrNO, int priviledgeMode, int isMultiCommit);
        if (dut_ptr->io_difftestCtrl_enable) {
          if (difftest_step(reg, dut_ptr->io_difftest_thisINST,
              dut_ptr->io_difftest_isMMIO, dut_ptr->io_difftest_isRVC, dut_ptr->io_difftest_isRVC2,
              dut_ptr->io_difftest_intrNO, dut_ptr->io_difftest_priviledgeMode, 
              dut_ptr->io_difftest_isMultiCommit)) {
#if VM_TRACE
            tfp->close();
#endif
            set_abort();
          }
        }
        lastcommit = n;
      }
      */

      uint32_t t = uptime();
      if (t - lasttime > 100) {
        poll_event();
        lasttime = t;
      }
    }
  }

  void cache_test(uint64_t n) {
    while (n > 0) {
      single_cycle();
      n --;
    }
  }

  void execute() {
//#define CACHE_TEST

#ifdef CACHE_TEST
    eprintf(ANSI_COLOR_MAGENTA "This is random test for cache.\n" ANSI_COLOR_RESET);
    cache_test(max_cycles);
#else
    execute_cycles(max_cycles);
#endif
  }
  uint64_t get_cycles() const { return cycles; }
  uint64_t get_max_cycles() const { return max_cycles; }
};
