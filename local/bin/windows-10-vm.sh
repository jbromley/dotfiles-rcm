#! /usr/bin/env bash
# Launch the Windows development VM
virsh --connect qemu:///system start windows-10-dev
virt-viewer --connect=qemu:///system --wait --full-screen windows-10-dev

