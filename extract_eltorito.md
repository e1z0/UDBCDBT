# Bootstrap boot image/sector from ISO file

```
./tools/geteltorito.pl -o boot.bin freedos.iso
cp boot.bin ./sys/
./build
```
