

# COOSCA处理器核快速上手教程

> COOSCA核隶属于国科大与计算所“一生一芯”项目, 采用Chisel语言模块化设计, 目前支持 RISCV32/64.



## 源代码结构

项目 Repository 建立在 Scala 框架下, 其中主要的的目录和文件如下

```
.
├── debug/             # 处理器核测试脚本
├── fpga/              # 用于FPGA平台调试运行的相关文件
├── doc/               # 项目文档
├── project/           # 构建SBT项目的相关配置文件
├── src/               # 处理器核源代码
├── script/            # 其他脚本文件
├── tool/              # 其他工具
├── Makefile           # Makefile
├── devlog.md          # 开发日志
└── README.md          # 项目介绍
```

./src 下的文件是项目最核心的处理器核源代码, 简要说明如下

```
./src
  ├── main/scala
  │    ├── bus         # 总线相关
  │    ├── device      # 输入输出设备相关
  │    ├── gpu         # GPU相关
  │    ├── noop        # 核心相关
  │    ├── system      # 外围系统
  │    ├── top         # 项目顶层文件
  │    └── utils       # 工具相关
  └── test
       ├── csrc        # C++测试文件, 主要用于Verilator仿真项目构建
       ├── vsrc        # Verilog测试文件
       └── scala       # Scala测试文件
```



## 准备工作

### 安装 Mill

* 请参考[该指南的Manual部分](https://www.lihaoyi.com/mill/#manual)

### 安装 GNU RISCV 工具链

Ubuntu 18.04 或者 Debian 10 以上: 

```bash
sudo apt-get install g++-riscv64-linux-gnu
```

其他平台

* 选项一: 使用我们预编译的[工具链](https://github.com/LvNA-system/labeled-RISC-V/releases/download/v0.1.0/riscv-toolchain-2018.05.24.tar.gz) (未测试)

* 选项二: 从源文件编译安装[工具链](https://github.com/riscv/riscv-tools)

### 安装 Verilator

Ubuntu 18.04 或者 Debian 10 以上：

```bash
sudo apt-get install verilator
```

其他平台

* 参考[官方教程](https://www.veripool.org/projects/verilator/wiki/Installing)从源文件编译安装



## 关联项目

> 运行和测试 COOSCA 核心还需要一些关联项目的辅助, 来提供核上运行时, 操作系统, 以及行为对比验证等

### NEMU

NEMU (NJU Emulator) 是一个简单但完整的全系统模拟器, 目前支持 x86, mips32, riscv32, riscv64 指令集. 在一生一芯项目中, NEMU 作为一个重要的基础设施, 被用来与处理器核作对比仿真验证.

* NEMU 项目的完整源码在 GitLab 上, 它的运行过程和相关概念请参考南京大学计算机系统基础课程的[PA0/PA1部分](https://nju-projectn.github.io/ics-pa-gitbook/ics2019/PA0.html).
* 处理器核中与对比验证相关的代码在 src/test/csrc/ 下的 difftest.cpp, difftest.h, emu.h 等文件中, 主要对比的是通用寄存器和CSR寄存器的值.

### AM

AM (Abstract Machine) 是一个向程序提供运行时环境的包装库, 它提供了一个面向裸金属的运行时环境, 把程序与体系结构进行了解耦. 我们只要在 AM 的框架下编写好程序, 就能方便地运行在 NEMU 和 NOOP 之上. AM 在一生一芯项目中被用来包装一系列测试程序从而验证核心的正确性.

* AM 项目的完整源码在 GitLab 上, 它的安装运行过程和相关概念请参考南京大学计算机系统基础课程的[PA2部分](https://nju-projectn.github.io/ics-pa-gitbook/ics2019/2.3.html).

### RISCV-PK & RISCV-LINUX

RISCV-PK (The RISC-V Proxy Kernel) 是一个轻量的 RISCV 运行时环境, 它能够将搭载程序的 I/O 系统请求代理到主机来完成. 我们项目中没有这样的需求, 而是主要使用了其中的 BBL (Berkeley Boot Loader) 部分, 它为 Linux 内核的运行提供预先准备, 包括注册 M mode 中断处理请求, 读取并解析设备树, 设置相应 CSR 寄存器值等等. 当 BBL 运行结束, 它就会将控制器正式交给内核.

在 GitLab 上面的 RISCV-PK 项目 Make 过程中, 会自动下载 RISCV-LINUX 这个项目工程, 这里面包含了一个最精简的 Linux 内核, ELF文件仅有1.4MB大小, 初始 init 为一个输出 Hello World 的小程序. 



## 仿真运行流程

该部分我们将以 Microbench 和 Linux 内核作为两个例子来说明如何进行处理器核的仿真运行.

### Microbench

Microbench 是一个建立在 AM 之上的基准测试程序, 位置在 nexus-am/apps/microbench/.

* 准备好 NOOP, NEMU, AM 这三个项目, 注意使用 git 切换到相应的分支, 做好 NOOP 的 Setting 工作
* 设置三个环境变量
  * `NEMU_HOME` = NEMU 项目的**绝对路径**
  * `NOOP_HOME` = NOOP 项目的**绝对路径**
  * `AM_HOME` = AM 项目的**绝对路径**

* 进入 nexus-am/apps/microbench/, 执行

  ```bash
  make ARCH=riscv64-nemu mainargs=test run
  ```

  该命令首先会使用 AM 运行时框架编译 Microbench 源代码, 形成一个内存映像二进制文件 (.bin), 然后让该文件载入进 NEMU 模拟器的内存中运行, 最后可以在终端中看到相应的输出以及最后的 Pass 字样. 

  其中, ARCH 指定 AM 的编译条件, `-` 之前部分指示指令集，`-` 之后部分指示运行平台; mainargs 指定目标程序的传入参数, 在 Microbench 中可以传入 "test", "train", "ref" 来设定测试规模.

* 进入 nexus-am/apps/microbench/, 执行

  ```bash
  make ARCH=riscv64-noop mainargs=test run
  ```

  该命令与上面唯一的不同是让 Microbench 运行到了 NOOP 的仿真平台之上, 如果顺利的话可以看到终端输出了和 NEMU 上一致的内容（除了运行时间有差异）.

  值得注意的是, 第一次运行该命令会从头开始 build NOOP 项目, 通常第一次 build 的时间会非常漫长, 这是因为 Mill 工具会下载依赖的 Scala/Java 包, 这些包在国内的网络环境中下载速度很慢. 一旦第一次构建成功后, 后续就不会重复下载了, build 速度会快很多.



### Linux Kernel

* 准备好 NOOP, NEMU, RISCV-PK 这三个项目, 注意使用 git 切换到相应的分支, 做好 NOOP 的 Setting 工作
* 设置两个环境变量
  * `NEMU_HOME` = NEMU 项目的**绝对路径**
  * `NOOP_HOME` = NOOP 项目的**绝对路径**

* 进入riscv-pk/, 执行

  ```bash
  rm dts/platform.dtsi; ln -s dts/nemu.dtsi dts/platform.dtsi
  ```

  建立一个软连接, 然后执行

  ```bash
  make nemu
  ```

  该命令会首先 clone 下来 riscv-linux 和 riscv-rootfs 两个项目, 编译出 Linux 内核 vmlinux 文件, 然后编译 BBL, 将内核与 BBL 组合成一个 ELF 文件, 之后转为内存映像二进制文件供 NEMU 运行. 静候一段时间, 可以在终端看到内核运行时的各种 Log, 最后会打印出来 Hello World 代表启动成功.

  需要注意, 在 riscv-pk 里面每次 make 之前要先手动删除 build/ 文件夹, 然后再新建一个空的 build/, 否则可能会出现编译不完全导致的意外错误.

* 进入riscv-pk/, 执行

  ```bash
  rm dts/platform.dtsi; ln -s dts/noop.dtsi dts/platform.dtsi
  ```

  建立一个软连接, 然后执行

  ```bash
  make noop
  ```

  与之前类似, 该项目会生成一个内存映像二进制文件, 但是运行在 NOOP 的仿真平台之上, 最终看到终端打印出来 Hello World 代表启动成功.



## FAQ

TODO