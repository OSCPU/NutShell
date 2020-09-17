# Stand-Alone Mode

In stand-alone mode, control is directly transferred to PL (Program Logic) through FSBL (First Stage BootLoader) after the board is powered on, so that PL has access to on-board peripherals such as SD card, Ethernet, etc., which is necessary to boot Debian and other OS.

We use PYNQ-Z2  board as example to demonstrate how to prepare SD card in stand-alone mode.

## Build BOOT.BIN

BOOT.bin is the default filename of packaged hardware-related binary files. [Here](pynq/BOOT.BIN) is a pre-built and currently-used BOOT.BIN.

You can also build it yourself. Please refer to the following process.

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



## Build RV_BOOT.bin

RV_BOOT.bin is the default filename of linux-kernel image. [Here](pynq/RV_BOOT.bin) is a pre-built and currently-used image. You can also build it yourself by riscv-pk and riscv-linux (currently not avaliable to the public).



## Build rootfs in SD Card

* New an `ext4` partition `mmcblk0p2` in SD card. Refer to the step of [here](https://wiki.debian.org/InstallingDebianOn/Xilinx/ZC702/wheezy#SD_Card_root) before executing `debootstrap`.

* Download the debian base system to `mmcblk0p2` with `qemu-debootstrap`.

```
sudo qemu-debootstrap --arch riscv64 unstable /mnt http://deb.debian.org/debian-ports
sudo chroot /mnt /bin/bash
passwd
apt-get update
apt-get install net-tools openssh-server vim build-essential minicom tmux libreadline-dev
exit
```

* Add a line of `ttyPS0` in `/mnt/etc/securetty` to allow login debian via `ttyPS0`. See [here](http://www.linuxquestions.org/questions/linux-newbie-8/login-incorrect-error-after-boot-no-password-prompted-881131/) for more details.

* Add a line of `PermitRootLogin yes` in `/mnt/etc/ssh/sshd_config` to enable root login via ssh. See [here](https://linuxconfig.org/enable-ssh-root-login-on-debian-linux-server) for more details.
* Add the following lines to `/mnt/etc/fstab`

```
# <file system> <mount point> <type>  <options> <dump>  <pass>
proc /proc proc defaults 0 0
/dev/mmcblk0p1 /boot vfat defaults 0 2
/dev/mmcblk0p2 / ext4 errors=remount-ro 0 1
```

Put `BOOT.BIN`, `RV_BOOT.BIN` generated in the previous steps into `/dev/mmcblk0p1`.
Finally, insert the SD card into the board. Pull down SW0 on pynq board to boot Debian.