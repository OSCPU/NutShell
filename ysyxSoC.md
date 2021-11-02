该项目支持 ysyxSoC 测试
1. 克隆 ysyxSoC 项目到 NutShell 同目录，并改名为 ysyxSoC-new
4. 修改 ysyxSoCFull.v 中实例化 CPU 的部分代码，把 ysyx_000000 改为 NutShell
6. 在 NutShell 里运行 make BOARD=soctest emu -j，然后就会自动运行测试
7. 可以通过 make BOARD=soctest emu -j IMAGE=path-to-bin 运行指定的测试

该项目支持直接生成流片的代码
1. 进入 NutShell，运行 make BOARD=out -j
