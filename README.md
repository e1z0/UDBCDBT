# Ultimate DOS Boot CD built tools (UDBCDBT) for Ultimate DOS Boot CD project (UDBCD)

https://user-images.githubusercontent.com/7213361/227746140-6f2ee743-00c1-4323-9a71-4e33247ed8cb.mov

## What is UDBCD (Ultimate DOS Boot CD ? 

It's wonderful thing you may use on your old vintage systems, maybe newer ones too. The included software is almost up to date

# How to craft a new ISO for UDBCD ?

**Currently only MacOS >=10.15 is supported as the host build system**

Clone this repository and build it!

```
git clone https://github.com/e1z0/UDBCDBT.git
make               # will use grub boot loader
make isolinux      # will use syslinux boot loader
make freedos       # if you want to run CD on computers with 2mb of ram
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

Steps to prepare floppy:

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
* grub4dos GPLv2
