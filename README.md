# Ultimate DOS Boot CD build tools (UDBCDBT) for Ultimate DOS Boot CD project (UDBCD)

https://user-images.githubusercontent.com/7213361/227746140-6f2ee743-00c1-4323-9a71-4e33247ed8cb.mov

# First release of UDBCD (alpha 1)

You can get it [here](https://archive.org/details/ultimate-dosboot-cd-grub-alpha-1-2023.03.29-17-30-43.7z).

# What is UDBCD (Ultimate DOS Boot CD) ? 

It's wonderful thing you may use on your old vintage systems, maybe newer ones too. The included software is almost up to date. This cdrom disk image contains all the necessary programs to restore, repair, test very old or newer computers and their components. With this disk, you can easily install MS-DOS versions 6.22 and 7.10, use the most popular file managers, create/clone disk partitions, make backup copies or restore the file system. There are usefull tools for loading external devices/peripherals. Drivers for ISCSI/USB/ZIP are included by default. Network support will come later.

# What is UDBCDBT ?

UDBCDBT stands for **Ultimate Dos Boot CD Build Tools**
It's a kind of framework or combined package of tools required to build live bootable cdrom image (.ISO). It uses this [format](loader_templates.md) for templates to define the boot loader menu entries which are located in **/cd/images** and this [format](apptemplate.md) to define the application templates which is located in **/cd/apps/apps.ini**. There is possibility to generate menu entries of the applications automatically, but you can't generate boot loader menu entries yet (it can only be defined with hand).

**Files for apps and apps/images/ content are not shared trough this git repository due to copyright infringements**. You can only use prebuild iso or get images/apps manually.

# How to craft a new UDBCD .ISO image ?

## System requirements for host build system (Linux)

* mkisofs (apt-get install genisoimage, pacman -S cdrtools)
* nasm (apt-get install nasm, pacman -S nasm)

## System requirements for host build system (macOS)

* mkisofs (brew install dvdrtools)

**Windows as a build host is supported via WSL2 only**

Clone this repository and build it!

```
git clone https://github.com/e1z0/UDBCDBT.git
make bootstrap     # Crafts the freedos bootable floppy image
make               # Makes iso with grub boot loader
make isolinux      # Makes iso with syslinux boot loader
make freedos       # Makes iso with boot straight to freedos (if you want to run CD on computers with 2mb of ram)
make test          # Will test live cd on qemu (without hdd). You must have qemu already installed on your system
make testhdd       # Will test live cd on qemu (with hdd). You must have qemu already installed on your system
```

The output .ISO image will be located in OUTPUT/ directory.

# Target System requirements

* At least 80386 or 80486 CPU (80286 could work but not tested)
* At least 2mb of ram (Plop boot manager may have video artifacts if using floppy > CD boot mode, but in general it works)

# How to boot old system that does not support booting from cd ?

You can use plop boot manager written to the floppy disk or hdd, more information read: https://www.plop.at/en/bootmanager/download.html
It supports booting from various media like USB, CD/DVD, Network using boot floppy/hdd as bootstrap.

## Steps to prepare floppy:

* Download https://download.plop.at/files/bootmngr/plpbt-5.0.15.zip
* Extract zip
* Run install\plpinst.com (to install to hdd)
* Or write install\plpbtin.img to a floppy using linux/macos (ex.: `sudo dd if=plpbtin.img of=/dev/fd0 bs=512 conv=sync;sync`)

# General software information

## Included boot loaders

* GRUB4DOS v0.4.6a (i think we will use older version of it because the new version does not boot on the system with 4mb of ram)
* SYSLINUX 6.03


## Included DOS Versions

* MS-DOS 6.22
* MS-DOS 7.10
* FreDOS 1.3 Lite USB edition (to save some space)

## Included Software

# License

This project is licensed Under BSD license, some parts of the project uses different software with different license forms. All copyrights are described in the copyrights section.

# Copyrights

* FreeDOS kernel - implements the core MS-DOS/PC-DOS (R) compatible operating system. It is derived from Pat Villani's DOS-C kernel and released under the GPL v2 or later. 
* DOS-C is (c) Copyright 1995, 1996 by Pasquale J. Villani All Rights Reserved.
* freedosbootdisks written by Jason Baker (jason@onejasonforsale.com)
* Grub4dos GPLv2
* Isolinux Copyright 1994-2008 H. Peter Anvin - All Rights Reserved
* MS-DOS (r) Copyright (c) Microsoft Corp.

All other trademarks are the property of their respective owners.
