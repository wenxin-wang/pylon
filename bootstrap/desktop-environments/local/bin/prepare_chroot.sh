#!/bin/bash

set -ex

mnt=$1

dir=$1
sudo mount -t proc /proc ${dir}/proc/
sudo mount --rbind --make-private /sys ${dir}/sys/
sudo mount --rbind --make-private /dev ${dir}/dev/
sudo mount --rbind --make-private /run ${dir}/run/
for i in proc sys dev run; do
    sudo mount --make-rslave ${dir}/"$i"
done
