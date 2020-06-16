#include <am.h>
#include <klib.h>
#include "runtime.h"

#define DMA_BASE 0x40003000
#define DMA_DEST (DMA_BASE + 0)
#define DMA_SRC  (DMA_BASE + 4)
#define DMA_LEN  (DMA_BASE + 8)

#define N 10
static int buf[N] = {};
static int buf2[N] = {};
static int buf3[N] = {};
#define SIZE (sizeof(buf))

static void dma(void *dest, const void *src, size_t n) {
  outl(DMA_DEST, (uint32_t)(uintptr_t)dest);
  outl(DMA_SRC, (uint32_t)(uintptr_t)src);
  printf("starting DMA with len = %d...\n", n);
  outl(DMA_LEN, n);

  printf("wait for finish...\n");
  while (inl(DMA_LEN) != 0);
}

int main() {
  difftest_disable();

  int i;
  for (i = 0; i < N; i ++) buf[i] = rand();
  dma(buf2, buf, SIZE);
  assert(memcmp(buf, buf2, SIZE) == 0);

  for (i = 0; i < N; i ++) buf3[i] = rand();
  dma(buf2, buf3, SIZE);
  assert(memcmp(buf3, buf2, sizeof(buf3)) == 0);

  return 0;
}
