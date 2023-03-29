# memtest

```
[general]
title = Memtest86+
file = /images/memtest
type = linux
method =
custom =
```

Will output to:

Isolinux:
```
LABEL memtest
 MENU LABEL Memtest86+
 kernel /images/memtest
```
Grub:
```
title Memtest86+
find --set-root /udbcd
kernel /images/memtest
```


# Linux

```
[general]
title = Damn Small Linux 4.4.10 [128MB RAM]
file = /images/dsllin24
type = linux
method = /images/dslrt24.gz
custom = ramdisk_size=58440 init=/etc/init lang=us apm=power-off vga=789 nomce noapic BOOT_IMAGE=dsl
```

Will output to:

Isolinux:
```
LABEL dsllin24
 MENU LABEL Damn Small Linux 4.4.10 [128MB RAM]
 kernel /images/dsllin24
 append initrd=/images/dslrt24.gz ramdisk_size=58440 init=/etc/init lang=us apm=power-off vga=789 nomce noapic BOOT_IMAGE=dsl
```
Grub:
```
title Damn Small Linux 4.4.10 [128MB RAM]
find --set-root /udbcd
kernel /images/dsllin24 ramdisk_size=58440 init=/etc/init lang=us apm=power-off vga=789 nomce noapic BOOT_IMAGE=dsl
initrd /images/dslrt24.gz
```


# DOS

```
[general]
title = FreeDOS Ultimate DOS Boot
file = /images/freedos.img
type = floppy
method =
custom =
```

Will output to:

Isolinux:
```
LABEL freedos.img
 MENU LABEL FreeDOS Ultimate DOS Boot
 kernel memdisk
 append initrd=/images/freedos.img floppy
```
Grub:
```
title FreeDOS Ultimate DOS Boot
find --set-root /udbcd
map /images/freedos.img (fd0)
map --hook
chainloader (fd0)+1
rootnoverify (fd0)
```


# DOS With memdisk

```
[general]
title = MS-DOS 6.22
file = /images/dos622.img
type = floppy
method = memdisk
custom =
```

Will output to:

Isolinux:
```
LABEL dos622.img
 MENU LABEL MS-DOS 6.22
 kernel memdisk
 append initrd=/images/dos622.img floppy
```
Grub:
```
title MS-DOS 6.22
find --set-root /udbcd
map --mem /images/dos622.img (fd0)
map --hook
chainloader (fd0)+1
rootnoverify (fd0)
```

# ISO (hd32 method)

```
[general]
title = Video Memory Stress Test
file = /images/memstrt.iso
type = iso
method = hd32
custom =
```

Will output to:

Isolinux:
```
LABEL memstrt.iso
 MENU LABEL Video Memory Stress Test
 kernel memdisk
 append initrd=/images/memstrt.iso iso
```
Grub:
```
title Video Memory Stress Test
find --set-root /udbcd
map /images/memstrt.iso (hd32) 
map --hook
chainloader (hd32)
boot
```
