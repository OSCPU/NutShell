#include "common.h"
#include "difftest.h"

#ifdef WITH_DRAMSIM3
#include "cosimulation.h"

CoDRAMsim3 *dram = NULL;
#endif

#define RAMSIZE (128 * 1024 * 1024)

static paddr_t ram[RAMSIZE / sizeof(paddr_t)];
static long img_size = 0;
void* get_img_start() { return &ram[0]; }
long get_img_size() { return img_size; }

void addpageSv39() {
//three layers
//addr range: 0x0000000080000000 - 0x0000000088000000 for 128MB from 2GB - 2GB128MB
//the first layer: one entry for 1GB. (512GB in total by 512 entries). need the 2th entries
//the second layer: one entry for 2MB. (1GB in total by 512 entries). need the 0th-63rd entries
//the third layer: one entry for 4KB (2MB in total by 512 entries). need 64 with each one all  

#define PAGESIZE (4 * 1024)  // 4KB = 2^12B
#define ENTRYNUM (PAGESIZE / 8) //512 2^9
#define PTEVOLUME (PAGESIZE * ENTRYNUM) // 2MB
#define PTENUM (RAMSIZE / PTEVOLUME) // 128MB / 2MB = 64
#define PDDENUM 1
#define PDENUM 1
#define PDDEADDR (0x88000000 - (PAGESIZE * (PTENUM + 2))) //0x88000000 - 0x1000*66
#define PDEADDR (0x88000000 - (PAGESIZE * (PTENUM + 1))) //0x88000000 - 0x1000*65
#define PTEADDR(i) (0x88000000 - (PAGESIZE * PTENUM) + (PAGESIZE * i)) //0x88000000 - 0x100*64
#define PTEMMIONUM 128
#define PDEMMIONUM 1

  uint64_t pdde[ENTRYNUM];
  uint64_t pde[ENTRYNUM];
  uint64_t pte[PTENUM][ENTRYNUM];
  
  //special addr for mmio 0x40000000 - 0x4fffffff
  uint64_t pdemmio[ENTRYNUM];
  uint64_t ptemmio[PTEMMIONUM][ENTRYNUM];
  
  pdde[1] = (((PDDEADDR-PAGESIZE*1) & 0xfffff000) >> 2) | 0x1;

  for(int i = 0; i < PTEMMIONUM; i++) {
    pdemmio[i] = (((PDDEADDR-PAGESIZE*(PTEMMIONUM+PDEMMIONUM-i)) & 0xfffff000) >> 2) | 0x1;
  }
  
  for(int outidx = 0; outidx < PTEMMIONUM; outidx++) {
    for(int inidx = 0; inidx < ENTRYNUM; inidx++) {
      ptemmio[outidx][inidx] = (((0x40000000 + outidx*PTEVOLUME + inidx*PAGESIZE) & 0xfffff000) >> 2) | 0xf;
    }
  }
  
  //0x800000000 - 0x87ffffff
  pdde[2] = ((PDEADDR & 0xfffff000) >> 2) | 0x1;
  //pdde[2] = ((0x80000000&0xc0000000) >> 2) | 0xf;

  for(int i = 0; i < PTENUM ;i++) {
    pde[i] = ((PTEADDR(i)&0xfffff000)>>2) | 0x1;
    //pde[i] = (((0x8000000+i*2*1024*1024)&0xffe00000)>>2) | 0xf;
  }

  for(int outidx = 0; outidx < PTENUM; outidx++ ) {
    for(int inidx = 0; inidx < ENTRYNUM; inidx++ ) {
      pte[outidx][inidx] = (((0x80000000 + outidx*PTEVOLUME + inidx*PAGESIZE) & 0xfffff000)>>2) | 0xf;
    }
  }

  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+PDEMMIONUM+PTEMMIONUM)),ptemmio, PAGESIZE*PTEMMIONUM);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+PDEMMIONUM)), pdemmio, PAGESIZE*PDEMMIONUM);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM)), pdde, PAGESIZE*PDDENUM);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDENUM)), pde, PAGESIZE*PDENUM);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*PTENUM), pte, PAGESIZE*PTENUM);
}

void init_ram(const char *img) {
  assert(img != NULL);
  FILE *fp = fopen(img, "rb");
  if (fp == NULL) {
    printf("Can not open '%s'\n", img);
    assert(0);
  }

  printf("The image is %s\n", img);

  fseek(fp, 0, SEEK_END);
  img_size = ftell(fp);

  fseek(fp, 0, SEEK_SET);
  int ret = fread(ram, img_size, 1, fp);
  assert(ret == 1);
  fclose(fp);

  //new add
  addpageSv39();
  //new end

  #ifdef WITH_DRAMSIM3
  assert(dram == NULL);
  dram = new CoDRAMsim3("/home/xyn/DRAMsim3/configs/DDR4_8Gb_x8_3200.ini", "build");
  #endif
}

extern "C" void ram_helper(
    paddr_t rIdx, paddr_t *rdata, paddr_t wIdx, paddr_t wdata, paddr_t wmask, uint8_t wen) {
  *rdata = ram[rIdx];
  if (wen) { ram[wIdx] = (ram[wIdx] & ~wmask) | (wdata & wmask); }
}

#ifdef WITH_DRAMSIM3

void dramsim3_helper(struct axi_channel &axi) {
  // currently only accept one in-flight read + one in-flight write
  static uint64_t raddr, roffset = 0, rlen;
  static uint64_t waddr, woffset = 0, wlen;

  // default branch to avoid wrong handsha
  axi.aw.ready = 0;
  // axi.w.ready  = 0;
  axi.b.valid  = 0;
  axi.ar.ready = 0;
  // axi.r.valid  = 0;

  // AXI read
  // first, check rdata in the last cycle
  if (axi.r.ready && axi.r.valid) {
    // printf("axi r channel fired\n");
    roffset--;
    axi.r.valid = 0;
  }
  // second, check whether we response data in this cycle
  if (roffset != 0) {
    axi.r.data = ram[raddr + (rlen - roffset)];
    axi.r.valid = 1;
    axi.r.last = (roffset == 1) ? 1 : 0;
  }
  // third, check ar for next request's address (since only one in-flight, we block the following requests)
  // put ar in the last since it should be at least one-cycle latency
  if (axi.ar.valid && roffset == 0) {
    axi.ar.ready = 1;
    // printf("axi ar channel fired\n");
    raddr = (axi.ar.addr % RAMSIZE) / sizeof(paddr_t);
    rlen = roffset = axi.ar.len + 1;
  }

  // AXI write
  // first, check wdata in the last cycle
  if (axi.w.valid && axi.w.ready) {
    // printf("axi w channel fired\n");
    ram[waddr + (wlen - woffset)] = axi.w.data;
    woffset--;
    axi.w.ready = 0;
  }
  if (woffset != 0) {
    axi.w.ready = 1;
  }
  if (axi.aw.valid && woffset == 0) {
    axi.aw.ready = 1;
    // printf("axi aw channel fired\n");
    waddr = (axi.aw.addr % RAMSIZE) / sizeof(paddr_t);
    wlen = woffset = axi.aw.len + 1;
  }
}

#endif
