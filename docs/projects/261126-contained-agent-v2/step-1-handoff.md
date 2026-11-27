# 第 1 步交接：迁设计并讨论（2026-11-26）

先读：

1. [README](README.md)：目标、待定、决定。
2. [TODO](TODO.md)：已迁和还剩的节。
3. [box-design](box-design.md)
4. uraj 的 [v2 README][uraj-v2]：还没迁的节。

## 现在在哪

| 环节 | 状态 | 代码 |
|---|---|---|
| 盒子 rootfs 原型（Guix profile、嵌套 Podman） | 实测通过 | uraj `exp/contained-agent-v2-box_20261115/` |
| 网络、reflink | 实测完 | uraj `exp/contained-agent-v2-workspace-box_20261115/` |
| 路线 C 原型 | 桌面 app 经测试主机 daemon 用盒子里的 workspace、终端、文件、diff；建 agent 未做 | uraj `exp/contained-agent-v2-route-c_20261115/` |
| 动态授权、设备授权（不用 root） | 实测通过；KVM 要用户先进 `kvm` 组 | uraj `exp/contained-agent-v2-grants_20261115/` |
| 正式代码 | 未开始 | |

## 下一步

迁下一节时：读 uraj README 里对应的条目和它引用的记录，改写进 box-design（决定和理由
分开写，理由留着）；凡提到 area、topic、space 里 project 的地方，按 11/26 的决定改掉，
改不了的列进待定问人。每个 PR 只迁一两节。

## 已知的坑

来自 uraj 里的实测，做原型或正式实现时还会碰到：

- 实测脚本都要人在主机终端跑：agent 会话碰不到 `contained-podman` 服务的 socket。输出
  写到 worktree 根目录的 `*.log` 让 agent 读，读完删掉；不要把输出重定向进 `exp/` 里的
  脚本文件。
- 跑过一次后 rootfs 里有属于 subordinate ID 的文件，主机用户删不掉，要
  `podman unshare rm -rf`。
- 盒子重建要删 `$PASEO_HOME/paseo.pid`，保留 `server-id`。
- 盒子里的 `paseo` CLI 要显式传 `--host`；主机上的 CLI 不带 `--host` 会连到人正在用的
  `~/.paseo`。
- 打包的 Paseo CLI 把 `PASEO_NODE_ENV` 写死成 `production`，mock provider 用不了；测
  agent 要真的 coding agent 和凭据。
- 桌面 app 升级后要彻底退出旧进程，否则一直是旧版本。
- 文档和脚本不写私有地址；`exp/` 脚本的地址都用参数传。

[uraj-v2]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/README.md
