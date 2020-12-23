#! /usr/bin/env bash
# Launch the Windows development VM
qdbus org.kde.KWin /KWin setCurrentDesktop 2
virsh --connect qemu:///system start ubuntu20.04
virt-viewer --connect=qemu:///system --wait --full-screen ubuntu20.04

