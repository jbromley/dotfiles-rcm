#! /usr/bin/env bash
# Launch the Windows development VM
virsh --connect qemu:///system start ubuntu-20.04
virt-viewer --connect=qemu:///system --wait --full-screen ubuntu-20.04
