# Stand-Alone Mode

In stand-alone mode, control is directly transferred to PL through fsbl (first stage bootloader) after the board is powered on, so that PL has access to on-board peripherals such as SD card, Ethernet, etc., which is necessary to boot Debian.

We use PYNQ-Z2  board as example to demonstrate how to prepare boot files in stand-alone mode.

## Build BOOT.BIN

* create a project in Vivado and generate bitstream

  ```
  cd $(NUTSHELL_HOME)/fpga
  make PRJ=myprj BOARD=pynq STANDALONE=true vivado
  ```

* generate hardware description file in Vivado

  ```
  Vivado -> File -> Export -> Export Hardware
  ```

* do bootgen initially

  ```
  cd $(NUTSHELL_HOME)/fpga
  make bootgen PRJ=myprj BOARD=pynq STANDALONE=true
  # this will report some error messages
  ```

* create project-related links

  ```
  cd $(NUTSHELL_HOME)/fpga/boot/build/zynq
  ln -sf $(NUTSHELL_HOME)/fpga/board/pynq/build/myprj-pynq/myprj-pynq.sdk/system_top_wrapper.hdf ps.hdf
  ln -sf $(NUTSHELL_HOME)/fpga/board/pynq/build/myprj-pynq/myprj-pynq.runs/impl_1/system_top_wrapper.bit fpga.bit
  # modify FSBL_LOC in $(NUTSHELL_HOME)/fpga/resource/fsbl-loader/Makefile like this:
  # FSBL_LOC = ../../boot/build/myprj-pynq/fsbl
  ```

* generate BOOT.BIN

  ```
  cd $(NUTSHELL_HOME)/fpga
  make bootgen PRJ=myprj BOARD=pynq STANDALONE=true
  ```



## Build RV_BOOT.BIN

RV_BOOT.BIN is the default filename of linux-kernel image. [Here](pynq/RV_BOOT.bin) is a pre-built and currently-used image. You can also build it yourself by riscv-pk and riscv-linux (zynq-standalone branch).



## Prepare SD Card

Please follow the instructions in [README](README.md) to build rootfs in SD card and then put `BOOT.BIN`, `RV_BOOT.BIN` generated in the previous steps into `/dev/mmcblk0p1`. 

Pull down SW0 on pynq board to boot Debian.
