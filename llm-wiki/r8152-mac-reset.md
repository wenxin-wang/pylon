---
description: Why an r8152 NIC with no stored MAC stops receiving after a USB reset, and how to work around it
category: hardware
sources:
  - https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c
  - https://github.com/torvalds/linux/blob/v7.3/drivers/net/usb/r8152.c
  - https://github.com/gregkh/linux/blob/v6.16.7/drivers/net/usb/r8152.c
  - https://github.com/torvalds/linux/blob/v6.16/net/core/dev.c
  - https://github.com/torvalds/linux/blob/v7.3/net/core/dev.c
  - https://github.com/torvalds/linux/blob/v6.16/net/core/rtnetlink.c
  - https://github.com/torvalds/linux/blob/v6.16/include/linux/etherdevice.h
  - https://github.com/torvalds/linux/commit/25766271e42f
  - https://github.com/torvalds/linux/commit/ad72c4a06acc
  - https://github.com/torvalds/linux/commit/0ec92a8f56ff
  - https://ratatoskr.run/linux-usb/2022/07/8882318/t
  - https://github.com/commaai/agnos-kernel-sdm845/pull/146
  - https://github.com/openwrt/openwrt/issues/9062
  - https://github.com/torvalds/linux/blob/8d7fe9885c62/MAINTAINERS
  - https://docs.kernel.org/admin-guide/reporting-issues.html
  - https://docs.kernel.org/admin-guide/reporting-regressions.html
  - https://docs.kernel.org/process/maintainer-netdev.html
  - https://docs.kernel.org/process/coding-assistants.html
  - https://docs.kernel.org/process/generated-content.html
checked: 2026-11-24
---

# r8152: random MAC is not written to the NIC after a USB reset

Linux `r8152` (Realtek RTL8152/8153 USB Ethernet). On a chip with no valid stored MAC, a
USB reset (Tx timeout, resume reset, `USBDEVFS_RESET`) leaves the kernel with a new
random address while the NIC's unicast filter is not updated. The NIC then receives
unicast traffic only in promiscuous mode. Present in v6.16, v6.16.7 and v7.3 (latest
release), and on 2026-11-24 in master (`8d7fe9885c62`, after v7.4-rc4), `net` and
`net-next`; no upstream fix or report of this exact symptom found.

Seen once on 2026-10-24 on an RTL8153B (kernel 6.16.7): after a Tx timeout and reset,
the interface received nothing until promiscuous mode was on; reading the chip registers
over usbfs showed the filter still held the old address. The register read and the
"stored MAC is all zeros" part are observations, not verified against source or
datasheet.

## Code path (identical at v6.16 and v7.3)

Line numbers are for v6.16 / v7.3; the functions below are textually identical in both
(and in master, 2026-11-22). `r8152.c` at the stable tag v6.16.7 is byte-identical to
v6.16.

1. A Tx timeout queues a USB reset: `rtl8152_tx_timeout()`
   ([L2851](https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c#L2851-L2858),
   [L3301](https://github.com/torvalds/linux/blob/v7.3/drivers/net/usb/r8152.c#L3301-L3308))
   calls `usb_queue_reset_device()`; the USB core then runs `rtl8152_pre_reset()` and
   `rtl8152_post_reset()`.
2. `rtl8152_post_reset()`
   ([L8422](https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c#L8422-L8461),
   [L8580](https://github.com/torvalds/linux/blob/v7.3/drivers/net/usb/r8152.c#L8580-L8619))
   calls `determine_ethernet_addr()` (once the device probed without errors) and, if
   that succeeds, `dev_set_mac_address(tp->netdev, &ss, NULL)`
   ([L8434-L8435](https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c#L8434-L8435),
   [L8592-L8593](https://github.com/torvalds/linux/blob/v7.3/drivers/net/usb/r8152.c#L8592-L8593)).
   The comment says "reset the MAC address in case of policy change" (commit
   25766271e42f, April 2019, first in v5.2-rc1: ACPI MAC pass-through can change at
   runtime and reset-based tools expect it to apply).
3. `determine_ethernet_addr()`
   ([L1788](https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c#L1788-L1824),
   [L2131](https://github.com/torvalds/linux/blob/v7.3/drivers/net/usb/r8152.c#L2131-L2167))
   tries platform data, then MAC pass-through, then `PLA_BACKUP` (RTL_VER_01 reads
   `PLA_IDR`). If the result is not a valid unicast address (zero or multicast) it logs
   `Invalid ether addr ...`, calls `eth_hw_addr_random(dev)`, copies `dev->dev_addr`
   into `ss`, logs `Random ether addr ...` and returns 0. `eth_hw_addr_random()` writes
   the new random address straight into `dev->dev_addr`
   ([etherdevice.h L274-L281](https://github.com/torvalds/linux/blob/v6.16/include/linux/etherdevice.h#L274-L281)).
   A new random address is drawn on every reset.
4. `dev_set_mac_address()` (a locking wrapper in `net/core/dev_api.c`) ends in
   `netif_set_mac_address()`
   ([v6.16 L9672](https://github.com/torvalds/linux/blob/v6.16/net/core/dev.c#L9672-L9696),
   [v7.3 L10078](https://github.com/torvalds/linux/blob/v7.3/net/core/dev.c#L10078-L10102)),
   which calls `ndo_set_mac_address` only `if (memcmp(dev->dev_addr, ss->__data, ...))`.
   Here `ss` equals `dev_addr` (step 3), so the driver hook is skipped. It still sets
   `addr_assign_type = NET_ADDR_SET` and sends `NETDEV_CHANGEADDR`.
5. The only place that writes the unicast filter register `PLA_IDR` is
   `__rtl8152_set_mac_address()`
   ([L1668](https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c#L1668-L1698),
   [L2011](https://github.com/torvalds/linux/blob/v7.3/drivers/net/usb/r8152.c#L2011-L2041)),
   reached from `ndo_set_mac_address`. So after step 4 nothing has written `PLA_IDR`;
   the kernel address and the hardware filter can differ.
6. `_rtl8152_set_rx_mode()`
   ([L2870](https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c#L2870-L2917))
   programs RCR with `RCR_APM` always and adds `RCR_AAP` only when `IFF_PROMISC` is set.
   By their names (the source doesn't say), APM accepts frames matching `PLA_IDR` and
   AAP accepts all. That is why promiscuous mode "fixes" reception. `post_reset`
   re-applies the rx mode (L8446), so the flag survives resets.

Probe and `reset_resume` do not have the problem: `set_ethernet_addr()`
([L1826](https://github.com/torvalds/linux/blob/v6.16/drivers/net/usb/r8152.c#L1826-L1842))
calls `__rtl8152_set_mac_address()` directly, with no `memcmp` in between (RTL_VER_01,
the RTL8152, only sets `dev_addr`).

## Since when

Before v6.5 `dev_set_mac_address()` always called the driver, so post_reset's call wrote
`PLA_IDR`. The `memcmp` skip was added by commit ad72c4a06acc "net: add check for
current MAC address in dev_set_mac_address" (Piotr Gardocki, June 2023, first in
v6.5-rc1; an optimisation for a bonding case, with redundant checks then removed from two
Intel drivers). 0ec92a8f56ff "net: fix net device address assign type" (`Fixes:
ad72c4a06acc`) narrowed the skip to the `ndo_set_mac_address` call only, so
`addr_assign_type` and the notifier still run. The bug therefore needs v6.5 or later,
plus a chip whose valid MAC cannot be read.

Side effect since v5.2: `post_reset` replaces a MAC set by userspace
with the one `determine_ethernet_addr()` returns, even on chips with a valid stored MAC
(a user-set MAC differs from it, so the driver hook does run and the stored MAC wins).

## Upstream status

No fix; the functions are unchanged between v6.16, v7.3 and master. No netdev /
linux-usb report of the missing `PLA_IDR` write was found. lore.kernel.org and patchwork
block scripted access (a proof-of-work page), so the search used a mirror (ratatoskr.run,
subject/sender only), bugzilla.kernel.org and web search; it is not exhaustive.

Related:

- July 2022, linux-usb, "handling MAC set by user space in reset_resume() of r8152"
  (Oliver Neukum; [mirror](https://ratatoskr.run/linux-usb/2022/07/8882318/t); the lore
  copy could not be opened). A user-set MAC is lost across resets. Hayes Wang (Realtek) agreed a manually set MAC should be kept
  through pre/post_reset and reset_resume, and noted a patch would conflict with
  25766271e42f. No patch was merged.
- A downstream kernel (commaai agnos-kernel-sdm845, PR 146, merged 2026-10-22, older
  `struct sockaddr` API) fixed the same family: an RTL8153 with no efuse MAC got a new
  random MAC on every reset (new DHCP client id, new IP). Its patch keeps the current
  `dev_addr` when the stored MAC is invalid. Not upstream, and it does not discuss
  `PLA_IDR`.
- OpenWrt issue 9062 (kernel 5.4, r8152 1.11/1.12): Tx timeout, then
  `Invalid ether addr 00:00:00:00:00:00`, `Random ether addr`, repeating. Same log
  signature; kernel 5.4 predates the v6.5 skip, so there the driver hook presumably ran.

## Workarounds

All follow from the source above; none was needed to change the driver.

- Recognise it: dmesg shows `Tx timeout`, then `Invalid ether addr 00:00:00:00:00:00`
  and `Random ether addr <new>` after each reset, and `ip link` shows a changed address.
- Re-set the wanted MAC after every reset: `ip link set dev <if> address <mac>`. This goes
  through the same `netif_set_mac_address()`, so it only reaches the driver (and writes
  `PLA_IDR`) if `<mac>` differs from the address `ip link` shows now. After a reset
  the shown address is random, so the wanted MAC is different and the write happens.
  Setting the displayed (random) address again is a no-op; to re-sync the hardware to a
  random address, set some other address first.
- Detect the reset without udev: `netif_set_mac_address()` sends `NETDEV_CHANGEADDR`
  even when it skips the driver, and `rtnetlink_event()` turns that into `RTM_NEWLINK`
  ([rtnetlink.c L6993](https://github.com/torvalds/linux/blob/v6.16/net/core/rtnetlink.c#L6993)),
  so `ip monitor link` shows the address change. A unit can watch it and re-set the MAC
  when the address differs from the wanted one.
- Blunt fallback: `ip link set dev <if> promisc on` (`RCR_AAP`), at the cost of
  receiving all frames. Link down/up does not help: nothing else writes `PLA_IDR`.
- Driver fix (not written): in `rtl8152_post_reset()` call `__rtl8152_set_mac_address()`
  as `set_ethernet_addr()` does, and/or keep the current `dev_addr` when no valid MAC
  can be read, as the downstream patch above does.

## Reporting it

From the kernel docs at master `8d7fe9885c62`; nothing has been sent.

- Recipients: MAINTAINERS has no r8152 entry. "USB NETWORKING DRIVERS" (`drivers/net/usb/`)
  lists only linux-usb, status Odd Fixes; "NETWORKING DRIVERS" adds netdev and five
  maintainers. `scripts/get_maintainer.pl` run in a real tree also adds recent signers
  from git history (likely Realtek's); run it rather than guessing.
- A report needs a fresh vanilla mainline kernel, untainted (`/proc/sys/kernel/tainted`
  is 0), with the version and dmesg, in plain text (reporting-issues).
- It may count as a regression (the driver hook ran before v6.5), but nobody has run a
  kernel before 6.5 on this hardware. A regression report wants the last good and first
  bad version, `[REGRESSION]` in the subject, `regressions@lists.linux.dev` in CC, a
  `#regzbot introduced: ...` line, and the culprit's signers in CC
  (reporting-regressions). Old regressions are handled case by case.
- netdev wants fixes as `[PATCH net]` with a `Fixes:` tag, and every fix must say in
  plain English how the bug was discovered and validated and on which hardware the fix
  was tested (maintainer-netdev).
- With an AI assistant (coding-assistants, generated-content): a non-trivial bug needs a
  reproducer, and writing and testing a fix "is not optional"; the agent adds no
  `Signed-off-by`, never sends anything itself, and says what it could not test; the
  human signs off, discloses the tool use, and must be able to defend the patch.
- So the human's part is: reproduce on mainline with the NIC, test a fix, sign off,
  send by plain-text mail, and answer on the list; an agent can draft the patch, the
  commit message and the reproduction steps.
