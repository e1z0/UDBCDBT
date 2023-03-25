# memtest

```
[general]
title = Memtest86+
file = /images/memtest
type = linux
method =
custom =
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

# DOS

```
[general]
title = FreeDOS Ultimate DOS Boot
file = /images/freedos.img
type = floppy
method =
custom =
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
