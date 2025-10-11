#!/bin/bash

set -e

WORKDIR=$(mktemp -d)

cleanup() {
    echo "Cleaning up..."
    rm -rf $WORKDIR
}

trap cleanup EXIT

TMPFILE=$WORKDIR/delegated-apnic-latest

echo "Downloading latest routes from APNIC..."
wget -O $TMPFILE https://ftp.apnic.net/apnic/stats/apnic/delegated-apnic-latest

echo "Generating routes..."
mkdir -p /etc/nftables/sets
echo 'elements = {' >/etc/nftables/sets/anpic-cn4.conf
awk -F '|' '/CN\|ipv4/{ printf("%s/%d,\n", $4, 32 - log($5)/log(2)) }' $TMPFILE >>/etc/nftables/sets/anpic-cn4.conf
echo '}' >>/etc/nftables/sets/anpic-cn4.conf

echo 'elements = {' >/etc/nftables/sets/anpic-cn6.conf
awk -F '|' '/CN\|ipv6/{ printf("%s/%d,\n", $4, $5) }' $TMPFILE >>/etc/nftables/sets/anpic-cn6.conf
echo '}' >>/etc/nftables/sets/anpic-cn6.conf

echo "Reloading nftable sets..."
sudo systemctl restart nftables
