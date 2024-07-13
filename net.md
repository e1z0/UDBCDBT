# Networking under dos

To enable TCP/IP networking support under UDBCD please boot FreeDOS and execute menu option Network > Univ. TCP/IP Network. It will detect ethernet interface on the machine and bring it up.
The universal packet driver will be installed at 0x60 address (most common). Now the other applications can access the network.

## Windows shares

Can be easily mounted using the following commands:
```
net use x: \\server\folder
net use x: \\server\folder2 /user:server\e1z0 Password
net use x: /delete
```
