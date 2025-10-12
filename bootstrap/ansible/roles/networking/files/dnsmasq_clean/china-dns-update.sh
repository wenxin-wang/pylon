#!/bin/bash

set -e

WORKDIR=$(mktemp -d)

cleanup() {
    echo "Cleaning up..."
    rm -rf $WORKDIR
}

trap cleanup EXIT

cd $WORKDIR

echo "Downloading latest TLD list..."
wget -O - https://data.iana.org/TLD/tlds-alpha-by-domain.txt | \
    sed -e 's@^\([^#].*\)@server=/\1/8.8.8.8@' \
        >/etc/dnsmasq.d/09-forward.tld.conf

echo "Downloading latest China list..."
wget -O dnsmasq-china-list.zip https://codeload.github.com/felixonmars/dnsmasq-china-list/zip/refs/heads/master
unzip dnsmasq-china-list.zip

cd dnsmasq-china-list-master

for _conf in accelerated-domains.china google.china apple.china; do
    awk -F '/' 'NF > 0 && !/^#/ { printf("server=/%s/#\nnftset=/%s/4#inet#basic#dns-cn4,6#inet#basic#dns-cn6\n", $2, $2) }' "$_conf.conf" >"/etc/dnsmasq.d/10-forward.$_conf.conf"
done
cp bogus-nxdomain.china.conf /etc/dnsmasq.d/10-bogus-nxdomain.china.conf

echo "Restarting dnsmasq..."
systemctl restart dnsmasq
