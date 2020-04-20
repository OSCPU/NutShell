#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdint.h>
#include <assert.h>
#include <string.h>

#ifdef __RV32__
typedef uint32_t rtlreg_t;
typedef uint32_t paddr_t;
#else
typedef uint64_t rtlreg_t;
typedef uint64_t paddr_t;
#endif

typedef uint64_t vaddr_t;
typedef uint16_t ioaddr_t;

#include "macro.h"

// 0~31: GPRs
enum {
  DIFFTEST_THIS_PC = 32,
#ifndef __RV32__
  DIFFTEST_MSTATUS,
  DIFFTEST_MCAUSE,
  DIFFTEST_MEPC,
  DIFFTEST_SSTATUS,
  DIFFTEST_SCAUSE,
  DIFFTEST_SEPC,
#endif
  DIFFTEST_NR_REG
};

#endif
