#include <am.h>
#include <klib.h>
#include "runtime.h"

#define MEIP_BASE 0x40004000
#define PLIC_BASE 0x3c000000
#define PLIC_ENABLE (PLIC_BASE + 0x2000)
#define PLIC_CLAIM  (PLIC_BASE + 0x200004)

int main() {
  uint32_t claim = inl(PLIC_CLAIM);
  assert(claim == 0);

  printf("enable meip\n");
  outl(MEIP_BASE, 1);
  claim = inl(PLIC_CLAIM);
  assert(claim == 0);

  outl(PLIC_ENABLE, 0x2);
  // the interrupt is not taken in this test,
  // and the execution flow continues

  claim = inl(PLIC_CLAIM);
  assert(claim == 1);

  uint32_t claim2 = inl(PLIC_CLAIM);
  assert(claim2 == 0);

  outl(PLIC_CLAIM, claim);

  // MEIP_BASE is still valid
  claim = inl(PLIC_CLAIM);
  assert(claim == 1);

  printf("disable meip\n");
  outl(MEIP_BASE, 0);
  printf("completion\n");
  outl(PLIC_CLAIM, claim);

  claim = inl(PLIC_CLAIM);
  assert(claim == 0);

  return 0;
}
