## enable swap on btrfs

```shell
$ sudo btrfs subvolume create /swap
$ sudo btrfs subvolume list /
$ btrfs filesystem mkswapfile --size 4g --uuid clear /swap/
$ sudo chattr +C /swap
$ sudo btrfs filesystem mkswapfile --size 16g --uuid clear /swap/swapfile
$ sudo swapon /swap/swapfile
$ sudo filefrag -v /swap/swapfile # do not use this for btrfs
$ sudo btrfs inspect-internal map-swapfile -r /swap/swapfile
the output is `resume_offset`
```

- how to check if btrfs subvol is enabled COW
```shell
# lsattr -d /path/to/directory
---------C-- /mydata   # 有 'C' 标志，CoW 已禁用
```

## Configure the initramfs
When using a busybox-based initramfs, the resume hook is required in /etc/mkinitcpio.conf. Whether by label or by UUID, the swap partition is referred to with a udev device node, so the resume hook must go after the udev hook. This example was made starting from the default hook configuration (before the systemd hook became the default):

```
HOOKS=(base udev autodetect microcode modconf kms keyboard keymap consolefont block filesystems resume fsck)
```

regenerate the initramfs
```
$ sudo mkinitcpio -P
```

## kernel parameters
```
title Arch Linux
linux /vmlinuz-linux
initrd /initramfs-linux.img
initrd /amd-ucode.img
options root=LABEL=chinslt rw rootflags=subvol=@ amd_iommu=off rtc_cmos.use_acpi_alarm=1 resume=UUID=<disk-uuid> resume_offset=<offset>
```

disk-uuid: check /etc/fstab, because I'm using the btrfs and subvol, so this value is / uuid
offset: sudo btrfs inspect-internal map-swapfile -r /swap/swapfile

```
sudo mkdir -p  /etc/systemd/system/systemd-homed.service.d/ && echo -e '[Service]\nEnvironment="SYSTEMD_HOME_LOCK_FREEZE_SESSION=false"' | sudo tee /etc/systemd/system/systemd-homed.service.d/override.conf 
for i in  /etc/systemd/system/systemd-{suspend,hibernate,hybrid-sleep,suspend-then-hibernate}.d; do
sudo mkdir -p $i && echo -e '[Service]\nEnvironment="SYSTEMD_SLEEP_FREEZE_USER_SESSIONS=false"' | sudo tee $i/override.conf
done
```
## sleep-then-hibernate
```
```
