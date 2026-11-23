---
description: Guix and Shepherd pitfalls: root umask/HOME, autofs in namespaces, herd restart args, Satty CJK
category: guix
sources:
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/gnu/build/activation.scm#L323-L335
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/guix/scripts/system.scm#L781-L788
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/guix/scripts/system/reconfigure.scm#L116-L123
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/guix/utils.scm#L1129-L1148
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/guix/git.scm#L83-L85
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/nix/libstore/local-store.cc#L343-L347
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/gnu/services/containers.scm#L329-L347
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/gnu/services/shepherd.scm#L112-L114
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/gnu/packages/fonts.scm#L2019-L2044
  - https://git.savannah.gnu.org/cgit/guile.git/tree/libguile/filesys.c?h=v3.0.9#n1285
  - https://github.com/sudo-project/sudo/blob/v1.9.17p1/plugins/sudoers/sudoers.c#L468-L473
  - https://github.com/sudo-project/sudo/blob/v1.9.17p1/plugins/sudoers/sudoers.c#L723-L731
  - https://github.com/sudo-project/sudo/blob/v1.9.17p1/plugins/sudoers/gram.y#L506-L508
  - https://github.com/sudo-project/sudo/blob/v1.9.17p1/plugins/sudoers/env.c#L904-L909
  - https://github.com/sudo-project/sudo/blob/v1.9.17p1/docs/sudoers.man.in#L4256
  - https://github.com/libgit2/libgit2/blob/v1.9.0/src/libgit2/index.c#L3864-L3867
  - https://github.com/containers/podman/blob/v6.1.4/vendor/go.podman.io/common/libnetwork/etchosts/hosts.go#L53-L57
  - https://github.com/containers/podman/blob/v6.1.4/vendor/go.podman.io/common/libnetwork/etchosts/util.go#L11-L15
  - https://github.com/containers/podman/blob/v6.1.4/pkg/rootless/rootless_linux.c#L185-L228
  - https://github.com/containers/podman/blob/v6.1.4/docs/source/markdown/podman-system-migrate.1.md
  - https://github.com/torvalds/linux/commit/922a6f34c1756d2b0c35d9b2d915b8af19e85965
  - https://github.com/torvalds/linux/blob/v6.19/fs/autofs/root.c#L344-L351
  - https://github.com/torvalds/linux/blob/v6.19/Documentation/filesystems/sharedsubtree.rst
  - https://github.com/torvalds/linux/blob/v6.12/fs/namespace.c#L3941-L3943
  - https://github.com/torvalds/linux/blob/v6.12/fs/namei.c#L1420-L1433
  - https://github.com/util-linux/util-linux/blob/v2.40.2/sys-utils/mount.8.adoc#L262-L271
  - https://github.com/systemd/systemd/blob/v257/src/shared/mount-setup.c#L436-L442
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/guix/uraj/system/base.scm#L81-L93
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/c/uraj/paseo/agent-supervisor.c#L84-L88
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/guix/storie-network-guide.md
  - https://codeberg.org/guix/guix/src/commit/b5a4d619c848bd00cbeec9d27d045112636881aa/guix/scripts/system/reconfigure.scm#L159-L160
  - https://codeberg.org/shepherd/shepherd/src/tag/v1.0.9/modules/shepherd/service.scm#L1097-L1155
  - https://codeberg.org/shepherd/shepherd/src/tag/v1.0.9/modules/shepherd/service.scm#L1165-L1175
  - https://codeberg.org/shepherd/shepherd/src/commit/a848fcf3e3bf929306bddf3e764fd61a7b214a97/modules/shepherd/service.scm#L1167-L1176
  - https://codeberg.org/shepherd/shepherd/src/commit/40edccbf04710913a811dfd5710afd9cb9ca808f/modules/shepherd/service.scm#L1165-L1175
  - https://codeberg.org/shepherd/shepherd/src/tag/v1.0.9/NEWS#L300-L310
  - https://codeberg.org/shepherd/shepherd/src/tag/v1.0.9/NEWS#L595-L599
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/guix/uraj/services/shepherd.scm
  - https://github.com/Satty-org/Satty/blob/v0.21.1/src/femtovg_area/imp.rs#L209-L231
  - https://github.com/Satty-org/Satty/blob/v0.21.1/README.md#L203-L206
  - https://github.com/Satty-org/Satty/blob/v0.21.1/README.md#L345
  - https://github.com/Satty-org/Satty/blob/v0.22.0/src/femtovg_area/imp.rs#L225-L235
  - https://github.com/femtovg/femtovg/blob/v0.25.0/src/text/font.rs#L213-L218
  - https://github.com/femtovg/femtovg/blob/v0.25.0/src/error.rs#L47-L50
  - https://github.com/harfbuzz/ttf-parser/blob/v0.25.1/src/lib.rs#L844-L853
  - https://gitlab.freedesktop.org/fontconfig/fontconfig/-/blob/2.17.1/src/fcfreetype.c#L1298-L1302
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/env/dotfiles/desktop/niri/.config/satty/config.toml
  - https://github.com/adileo/squirreldisk/issues/84
checked: 2026-11-23
---

# Guix System and Shepherd pitfalls

Things that cost time on Guix System. Checked against Guix master at commit `b5a4d61`
(2026-11-23; Shepherd 1.0.9, sudo 1.9.17p1, Guile 3.0.9) and Podman 6.1.4, unless a
section says otherwise. Behaviour on deployed systems was observed once each.

NFS exports and the on-demand autofs mounts on Guix (direct autofs entries are rejected
by `automount`, so shares are mounted under `/mnt/storie/` and the old paths are
symlinks) are already written up in uraj's
[storie-network-guide.md](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/guix/storie-network-guide.md);
not repeated here.

## Root-run Guix inherits umask and HOME

**Symptom.** After `sudo guix system reconfigure` from a shell with `umask 077`,
rootless Podman fails with `failed to create new hosts file: open /etc/hosts:
permission denied`, and tools that copy `~/.cache/guix` fail with permission denied on
`.git/index`.

**Why.**

- `guix system reconfigure` runs the activation script inside the `guix` process
  itself (`local-eval`: "Evaluate EXP, a G-Expression, in-place"), so it runs with the
  umask of whoever started it. sudo does not reset a stricter umask: the effective
  umask is the user's OR-ed with sudoers' `umask` (default 022); only
  `Defaults umask_override` changes that. Guix's sudo package keeps the defaults.
- `activate-etc` makes every `/etc` entry a symlink into the store except `sudoers`
  (copied, then `chmod 440`) and `/etc/hosts`, which is `copy-file`d so that software
  can write it. Guile's `copy-file` creates the target with `open(2)` using the source
  mode, so the umask applies; store files are 0444. With umask 077 `/etc/hosts` becomes
  `0400 root`, and Podman, reading it as the unprivileged user (it is the default
  `base_hosts_file`), fails. The file is recreated on every activation.
- With `sudo -E`, HOME stays the caller's: `-E` with the `setenv` privilege (implied
  by a `... ALL` command, which Guix's default `%wheel ALL=(ALL) ALL` is) turns off
  `env_reset`, and HOME is only reset to the target's with `-H`, `always_set_home` or a
  login shell. Guix puts its cache in `$XDG_CACHE_HOME/guix`, else `$HOME/.cache/guix`,
  and channel checkouts under `.../guix/checkouts`. So a `guix pull` or
  `guix time-machine` run as root with `sudo -E` writes root-owned files into the
  user's cache. libgit2 (what Guix uses for checkouts) creates the index with mode
  0666 minus umask: 0600 under umask 077, so the user cannot even read it.

**Fix.**

- Do not set a restrictive umask globally. Run reconfigure with a normal one, or
  `umask 022` in the wrapper that calls sudo.
- Give root its own cache: `sudo -E XDG_CACHE_HOME=/root/.cache ...` or `sudo -E -H ...`
  (HOME of the target user), or skip `-E`.
- Recovery: `sudo chmod 644 /etc/hosts` (or reconfigure again under umask 022; Podman
  needs no restart) and chown back what root wrote, e.g.
  `sudo find ~/.cache/guix -user root -exec chown "$USER": {} +`.

## autofs/NFS and mount propagation

**Symptom.** A path on an autofs/NFS share works in the login shell but not inside a
sandbox or rootless Podman container: `stat` fails with EPERM, or the share is missing
(a launcher that tests the path first just skips it). Inside the namespace the share's
autofs mount point is there but without the NFS mount.

**Why.**

- The kernel default is a private `/`. systemd makes it recursively shared at boot
  (`mount(NULL, "/", NULL, MS_REC|MS_SHARED, NULL)`, unless run in a container);
  Guix does not.
- Guix's `rootless-podman-service-type` has a one-shot service doing only
  `mount --make-shared /` (non-recursive), requiring `user-processes`. A non-recursive
  call changes one mount; mounts that exist at that point, such as an autofs mount point
  from boot, keep their propagation (util-linux `mount(8)`, `--make-rshared` is the
  recursive form).
- A cloned mount namespace keeps each mount's type (private stays private, shared
  becomes a peer, slave stays slave). The automount daemon mounts the NFS in its own
  namespace; a private autofs mount in another namespace never receives it. When the
  namespace is created together with a user namespace (rootless Podman uses
  `clone(CLONE_NEWUSER|CLONE_NEWNS)`, so do bwrap and `unshare -U -m`), shared mounts
  are turned into slaves (`CL_SHARED_TO_SLAVE`), which still receive the host's later
  mounts. A plain `unshare(CLONE_NEWNS)` keeps them shared (peers).
- Linux 6.19 and later refuse the trigger outright: `autofs_d_automount` returns
  `-EPERM` when the caller's mount namespace is not the daemon's and the autofs mount
  is propagation private (commit 922a6f34c175, absent in 6.18). Before that the path
  walk looped until `MAXSYMLINKS`, per the commit message. Observed on kernel 7.x.

**Fix.**

- Make `/` recursively shared before any service starts: a Guix activation service
  doing `mount(2)` with `MS_REC | MS_SHARED` (activation also runs at reconfigure, so no
  reboot is needed), or `mount --make-rshared /`. Mounts made later under it inherit
  that.
- A supervisor that creates a mount namespace should mark it `MS_REC | MS_SLAVE`, not
  `MS_PRIVATE`: its own mounts (a fresh `/proc`) stay inside, host mounts and autofs
  triggers still reach it. Slaving a mount that is not shared does nothing
  (sharedsubtree.rst), so this only works once `/` is shared.
- Cost of a shared `/`: a `mount` or `umount` inside a namespace created with
  `CLONE_NEWNS` and not made private or slave propagates back to the host.
- uraj's fix:
[`system/base.scm`](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/guix/uraj/system/base.scm#L81-L93),
[`agent-supervisor.c`](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/c/uraj/paseo/agent-supervisor.c#L84-L88).

**Existing namespaces stay as they were.** Rootless Podman saves its user and mount
namespaces as nsfs file handles and rejoins them with `open_by_handle_at`
(`ns_handles`), so while any process (pause process, conmon, a container, the
`podman system service`) still lives in the old namespace, a restarted Podman rejoins
it. Podman's own docs say the same for the pause process
(`podman system migrate`). To pick up the new propagation: stop every container and
runtime of that user, restart the Podman service, and check that the pause process is
in a new mount namespace (`readlink /proc/PID/ns/mnt`) whose `/proc/PID/mountinfo`
line for the share has `master:N`. Killing only the pause process, or only restarting
the service, was not enough (observed with Podman 6.1.x).

## `herd restart SERVICE ARG` drops ARG after a reconfigure

Shepherd 1.0.9, which is Guix's default (`shepherd-configuration`). `guix system
reconfigure` does not restart running services: the new definition is registered as a
*replacement* of the running one (Guix then prints "run 'herd restart SERVICE' ...").
Shepherd's built-in `restart` forwards ARGS to `start-service` only for `(eq? restarting service)`,
but `stop-service` returns `(cons (or replacement service) dependents)`: with a pending
replacement the object started is not the one stopped, so it is started without ARGS.
The first restart after a reconfigure silently ignores the args (forwarding args on
restart exists only since Shepherd 1.0.4, NEWS).

- Check: `herd status SERVICE` shows whether a replacement is pending (since 1.0.0, NEWS).
- Workaround: do a restart without args first (installs the replacement), then
  `herd restart SERVICE ARG`.
- Not fixed in Shepherd `main` (1.0.9 plus four commits, 2026-06-22) or `devel`
  (a848fcf, 2026-11-22, where the same logic moved into `restart-service`). No upstream
  report found.
- uraj defines its own `restart` action that starts `(car (stop-service ...))` with the
  args:
  [`services/shepherd.scm`](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/guix/uraj/services/shepherd.scm).
  It only takes effect once the new definition is loaded, so the first restart after
  deploying it still drops the args.

## Satty cannot draw Chinese with Noto Sans CJK from Guix

Satty (screenshot editor) 0.21.1 draws text with femtovg and loads only the fonts named
in its config ("no proper fallback font handling", README); `[font] fallback` exists
since 0.20.1. With no CJK font there, Chinese typed through an input method has no
glyphs. The obvious fix fails:

```toml
[font]
family = "Noto Sans"
fallback = ["Noto Sans CJK SC"]   # "Fallback font: Failed to load font: canvas error"
```

Working, observed once on 2026-11-04 (not run end to end here):
`fallback = ["Sarasa Gothic SC"]` (static). uraj's config:
[`satty/config.toml`](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/env/dotfiles/desktop/niri/.config/satty/config.toml).

**Cause (code-verified, reproduced up to fontconfig, Satty itself not run).**

- Guix's `font-google-noto-sans-cjk` 2.004 ships only variable fonts: `out` holds
  `NotoSansCJK-VF.otf.ttc` (5 faces, each with 7 named instances), output `otf` the
  per-language variable `.otf`.
- fontconfig's `FC_INDEX` for a named instance is `(instance + 1) << 16 | face`.
  `fc-match -f '%{index}' "Noto Sans CJK SC"` on this package, fontconfig 2.17.1,
  prints `262146` (Regular, face 2); Sarasa Gothic SC prints `1`.
- Satty 0.21.1 (and 0.22.0, unchanged) passes `font.index` unmasked to femtovg's
  `add_shared_font_with_index`; femtovg calls `ttf_parser::Face::parse(data, 262146)`,
  which fails with `FaceIndexOutOfBounds` (5 faces), and femtovg prints every
  `ErrorKind` as "canvas error". So "canvas error" says nothing about the real cause.
- Another program hit the same number: SquirrelDisk issue 84 (egui/epaint panic
  `Invalid index 262146 for font collection`, Noto Sans CJK SC on Fedora); installing
  the static Noto package gave index 2. That reporter's explanation (packed
  named-instance index) is a hypothesis; the fontconfig source above confirms the
  packing.

The fix on the Satty side would be masking the index (`& 0xFFFF`); no Satty issue found.
fontconfig also lists the variable font itself with the plain index (2, `variable=True`),
but style matching picks the Regular named instance. A static Noto Sans CJK would avoid
this; Guix has one only for Serif (`font-google-noto-serif-cjk-static`).
