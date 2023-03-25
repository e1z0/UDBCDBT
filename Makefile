HD := freedos.img
BOOTORDER := cd

all: build

freedos: bootstrap
	./tools/genmenus.pl --bootloader freedos --bloaderpath null --images null --apps ./cd/apps --menu ./cd/utils/pcms130
	./build freedos
isolinux:
	./tools/genmenus.pl --bootloader isolinux --bloaderpath ./cd/isolinux --images ./cd/images --apps ./cd/apps --menu ./cd/utils/pcms130
	./build isolinux

bootstrap:
	./bootstrap

build:
	./tools/genmenus.pl --bootloader grub --bloaderpath ./cd/grub --images ./cd/images --apps ./cd/apps --menu ./cd/utils/pcms130
	./build
test:
	qemu-system-i386 -cdrom ./OUTPUT/`cat .LATEST` -m 32
testhdd:
        ifneq ("$(wildcard $(HD))","")
		@echo "HDD image already exists"
        else
		@echo "HDD image does not exist, creating..."
		qemu-img create freedos.img 50M
        endif
	qemu-system-i386 -boot order=$(BOOTORDER),menu=on,splash-time=5 -drive file=$(HD),id=disk,format=raw -drive file=./OUTPUT/`cat .LATEST`,media=cdrom -hdc fat:rw:$$(pwd)/share -m 32
clean:
	find . -name .DS_Store -exec rm {} \;
	rm OUTPUT/*
scanapp:   
	./tools/freedos_menu.pl --apps ./cd/apps --scan
genapp:
	./tools/freedos_menu.pl --apps ./cd/apps --menu ./cd/utils/pcms130

.PHONY: build bootstrap test testhdd
