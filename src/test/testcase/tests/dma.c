#include <am.h>
#include <klib.h>
#include "runtime.h"

#define DMA_BASE 0x40003000
#define DMA_DEST (DMA_BASE + 0)
#define DMA_SRC  (DMA_BASE + 4)
#define DMA_LEN  (DMA_BASE + 8)

#define N 8192
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

static inline int gen_data(int i) {
  return rand();
//  uint8_t byte0 = (i + 0) & 0xff;
//  uint8_t byte1 = (i + 1) & 0xff;
//  uint8_t byte2 = (i + 2) & 0xff;
//  uint8_t byte3 = (i + 3) & 0xff;
//  return byte0 | (byte1 << 8) | (byte2 << 16) | (byte3 << 24);
}

static inline void check_buf(int *ref, int *dut, int size) {
  int i;
  for (i = 0; i < size; i ++) {
    if (ref[i] != dut[i]) {
      printf("wrong at idx = %d, right = 0x%08x, wrong = 0x%08x\n", i, ref[i], dut[i]);
      assert(0);
    }
  }
}

int main() {
  int i;
  for (i = 0; i < N; i ++) buf[i] = gen_data(i);
  dma(buf2, buf, SIZE);
  check_buf(buf, buf2, N);

  for (i = 0; i < N; i ++) buf3[i] = gen_data(i);
  dma(buf2, buf3, SIZE);
  check_buf(buf2, buf3, N);

  return 0;
}
