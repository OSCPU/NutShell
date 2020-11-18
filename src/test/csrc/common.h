#ifndef __COMMON_H
#define __COMMON_H

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <stdint.h>
#include <assert.h>

#ifdef __cplusplus
# include <stdexcept>
# define print_and_die(s) throw std::runtime_error(s)
#else
# define print_and_die(s) do { fprintf(stderr,"%s\n",s); abort(); } while(0)
#endif

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

#define eprintf(...) fprintf(stderr, ## __VA_ARGS__)

#define demand(cond,str,...) \
  do { if(!(cond)) { \
      char __str[256]; \
      snprintf(__str,256,"in %s, line %d: " str, \
               __FILE__,__LINE__,##__VA_ARGS__); \
      print_and_die(__str); \
    } } while(0)

// for debugging
int sc(unsigned int ncycle, int *ret_code);
int si(unsigned int ninstr, int *ret_code);
unsigned int read_reg(int reg_no);

// device
void init_device(void);
bool is_finished(void);
int get_exit_code(void);

// log
enum {
  LOG_ALL = 0,
  LOG_DEBUG,
  LOG_INFO,
  LOG_WARN,
  LOG_ERROR,
  LOG_OFF
};

uint64_t getLogLevel(const char * str);

void app_error(const char *fmt, ...);

int monitor(void);

#ifdef WITH_DRAMSIM3

#define AXI_ADDR_TYPE uint32_t
#define AXI_DATA_TYPE uint64_t


struct axi_aw_channel {
  uint8_t       ready;
  uint8_t       valid;
  AXI_ADDR_TYPE addr;
  uint8_t       prot;
  uint8_t       id;
  uint8_t       user;
  uint8_t       len;
  uint8_t       size;
  uint8_t       burst;
  uint8_t       lock;
  uint8_t       cache;
  uint8_t       qos;
};

struct axi_w_channel {
  uint8_t       ready;
  uint8_t       valid;
  AXI_DATA_TYPE data;
  uint8_t       strb;
  uint8_t       last;
};

struct axi_b_channel {
  uint8_t       ready;
  uint8_t       valid;
  uint8_t       resp;
  uint8_t       id;
  uint8_t       user;
};

struct axi_ar_channel {
  uint8_t       ready;
  uint8_t       valid;
  AXI_ADDR_TYPE addr;
  uint8_t       prot;
  uint8_t       id;
  uint8_t       user;
  uint8_t       len;
  uint8_t       size;
  uint8_t       burst;
  uint8_t       lock;
  uint8_t       cache;
  uint8_t       qos;
};

struct axi_r_channel {
  uint8_t       ready;
  uint8_t       valid;
  uint8_t       resp;
  AXI_DATA_TYPE data;
  uint8_t       last;
  uint8_t       id;
  uint8_t       user;
};


struct axi_channel {
  struct axi_aw_channel aw;
  struct axi_w_channel  w;
  struct axi_b_channel  b;
  struct axi_ar_channel ar;
  struct axi_r_channel  r;
};

// dut helper for AXI

// NOTE: change this when migrating between different hardware designs
#define DUT_AXI(name) io_mem_##name

#define axi_aw_copy_from_dut_ptr(dut_ptr, aw)    \
  do {                                           \
    aw.ready = dut_ptr->DUT_AXI(aw_ready);       \
    aw.valid = dut_ptr->DUT_AXI(aw_valid);       \
    aw.addr = dut_ptr->DUT_AXI(aw_bits_addr);    \
    aw.prot = dut_ptr->DUT_AXI(aw_bits_prot);    \
    aw.id = dut_ptr->DUT_AXI(aw_bits_id);        \
    aw.user = dut_ptr->DUT_AXI(aw_bits_user);    \
    aw.len = dut_ptr->DUT_AXI(aw_bits_len);      \
    aw.size = dut_ptr->DUT_AXI(aw_bits_size);    \
    aw.burst = dut_ptr->DUT_AXI(aw_bits_burst);  \
    aw.lock = dut_ptr->DUT_AXI(aw_bits_lock);    \
    aw.cache = dut_ptr->DUT_AXI(aw_bits_cache);  \
    aw.qos = dut_ptr->DUT_AXI(aw_bits_qos);      \
  } while (0);

#define axi_w_copy_from_dut_ptr(dut_ptr, w)      \
  do {                                           \
    w.ready = dut_ptr->DUT_AXI(w_ready);         \
    w.valid = dut_ptr->DUT_AXI(w_valid);         \
    w.data = dut_ptr->DUT_AXI(w_bits_data);      \
    w.strb = dut_ptr->DUT_AXI(w_bits_strb);      \
    w.last = dut_ptr->DUT_AXI(w_bits_last);      \
  } while (0);

#define axi_b_copy_from_dut_ptr(dut_ptr, b)      \
  do {                                           \
    b.ready = dut_ptr->DUT_AXI(b_valid);         \
    b.valid = dut_ptr->DUT_AXI(b_valid);         \
    b.resp = dut_ptr->DUT_AXI(b_bits_resp);      \
    b.id = dut_ptr->DUT_AXI(b_bits_id);          \
    b.user = dut_ptr->DUT_AXI(b_bits_user);      \
  } while (0);

#define axi_ar_copy_from_dut_ptr(dut_ptr, ar)    \
  do {                                           \
    ar.ready = dut_ptr->DUT_AXI(ar_ready);       \
    ar.valid = dut_ptr->DUT_AXI(ar_valid);       \
    ar.addr = dut_ptr->DUT_AXI(ar_bits_addr);    \
    ar.prot = dut_ptr->DUT_AXI(ar_bits_prot);    \
    ar.id = dut_ptr->DUT_AXI(ar_bits_id);        \
    ar.user = dut_ptr->DUT_AXI(ar_bits_user);    \
    ar.len = dut_ptr->DUT_AXI(ar_bits_len);      \
    ar.size = dut_ptr->DUT_AXI(ar_bits_size);    \
    ar.burst = dut_ptr->DUT_AXI(ar_bits_burst);  \
    ar.lock = dut_ptr->DUT_AXI(ar_bits_lock);    \
    ar.cache = dut_ptr->DUT_AXI(ar_bits_cache);  \
    ar.qos = dut_ptr->DUT_AXI(ar_bits_qos);      \
  } while (0);

#define axi_r_copy_from_dut_ptr(dut_ptr, r)      \
  do {                                           \
    r.ready = dut_ptr->DUT_AXI(r_ready);         \
    r.valid = dut_ptr->DUT_AXI(r_valid);         \
    r.resp = dut_ptr->DUT_AXI(r_bits_resp);      \
    r.data = dut_ptr->DUT_AXI(r_bits_data);      \
    r.last = dut_ptr->DUT_AXI(r_bits_last);      \
    r.id = dut_ptr->DUT_AXI(r_bits_id);          \
    r.user = dut_ptr->DUT_AXI(r_bits_user);      \
  } while (0);

#define axi_copy_from_dut_ptr(dut_ptr, axi)      \
  do {                                           \
    axi_aw_copy_from_dut_ptr(dut_ptr, axi.aw)    \
    axi_w_copy_from_dut_ptr(dut_ptr, axi.w)      \
    axi_b_copy_from_dut_ptr(dut_ptr, axi.b)      \
    axi_ar_copy_from_dut_ptr(dut_ptr, axi.ar)    \
    axi_r_copy_from_dut_ptr(dut_ptr, axi.r)      \
  } while (0);

void dramsim3_helper(struct axi_channel &axi);
#endif

#endif // __COMMON_H
