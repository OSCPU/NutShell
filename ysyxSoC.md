该项目支持 ysyxSoC 测试 (但有一些 dirty work)
1. 克隆 ysyxSoC 项目到 NutShell 同目录，并改名为 ysyxSoC-new
2. ysyxSoC 项目切换到 38eba9b397d10f0eadef831193c74c 这一 commit
3. 到 ysyxSoC/ysyx/peripheral/spiFlash/ 里面写一个 spiFlash.h, 内容是 “extern "C" void flash_init(char *img);”
4. 修改 ysyxSoCFull.v 中实例化 CPU 的部分代码，把 ysyx_000000 改为 NutShell，然后一些 AXI 信号视情况可能要做一些微调 (比如 io_master_awready 改为 io_master_aw_ready)，可以根据报错 lazy 地更改
5. 进入 NutShell/src/test/csrc/emu.h，在 87 行处修改要跑的 flash 镜像
6. 在 NutShell 里运行 make BOARD=soctest emu -j，然后就会自动运行测试

该项目支持直接生成流片的代码
1. 进入 NutShell，运行 make BOARD=out -j