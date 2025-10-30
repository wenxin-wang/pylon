#!/bin/bash

persisted_dir=/etc/nftables/persisted

if [ -f /etc/nftables/persisted-sets.txt ]; then
    dir=$persisted_dir/sets
    mkdir -p $dir

    while read -r nfproto table set; do
        nft list set $nfproto $table $set >$dir/$nfproto.$table.$set.conf
    done </etc/nftables/persisted-sets.txt
fi

if [ -f /etc/nftables/persisted-maps.txt ]; then
    dir=$persisted_dir/maps
    mkdir -p $dir

    while read -r nfproto table map; do
        nft list map $nfproto $table $map >$dir/$nfproto.$table.$map.conf
    done </etc/nftables/persisted-maps.txt
fi
