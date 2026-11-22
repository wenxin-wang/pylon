---
description: 手机上审批 sudo 和 agent 特权请求：现成方案为什么不够，自建的几个方向和分级
category: security
sources:
  - 2026-11-02 的两次 agent 调研会话（记录已清理，这里是 2026-11-14 的摘录）
checked: 2026-11-02
---

# 手机审批 sudo / agent 特权请求

调研时的建议，不是已采纳的决定。

## 结论

没有现成方案同时做到成熟、自托管、手机上能看到具体命令。

- 看不到命令的批准只是确认“有人要 sudo”：同用户的恶意程序可以抢在人正常 sudo 时
  发起请求，提示一模一样。只省了输密码，没有审计作用。
- 走 PAM 的方案结构性受限：PAM 只管认证，看不到 polkit action 和命令。

## 看过的方案

| 方案 | 问题 |
|---|---|
| Duo Unix (pam_duo) | 依赖 Cisco 云；pushinfo 读 `SUDO_COMMAND`，可能为空或被伪造；polkit 下基本不显示 |
| pam_oath (TOTP) | 不显示命令；验证码也在手机上 |
| pam_authelia / privacyIDEA | 无证据显示命令，要跑服务 |
| sudostamp + pam_rssh | 明确不显示命令；新项目未审计 |
| sudo-approve (ntfy + WebAuthn 网页) | 个人项目，未说明显示命令；结构可借鉴 |
| PushGate | 太新，要专用 app |
| sudo_pair (Square) | 审批者连本地 socket，不适合手机；2023 后无更新 |
| passkey (pam_u2f，手机当 FIDO2) | 不显示命令 |
| sudo 1.9 approval plugin (Python) | 能看到命令，但 Guix 的 sudo 没编 python_plugin；需 NOPASSWD，插件加载失败时 sudo 变无密码 |
| run0/polkit、Teleport、agsudo 等 | 要 systemd / 企业产品 / 只有本地 TUI / 启发式打分 |
| fanotify、auditd、AppArmor | 无 argv / 只能事后记录 / Guix 上难用 |

## 自建的方向（按讨论先后）

1. 薄审批服务：`sudo → pam_exec → 本机审批服务 → ntfy 推送（不带内容）`，手机打开网页
   看命令和级别，WebAuthn 确认；超时回落到密码。手机经 wireguard 访问，WebAuthn 需要
   DNS-01 证书，这是最麻烦的部分。密码仍要保留，所以安全性没提高，只是省输入、多了审计。
2. 新命令 `pk`：先提交命令审批，批准后生成一次性 permit，再 `pkexec`；polkit 规则校验
   permit，否则 `AUTH_ADMIN`。sudo 不动，作为回落。代价：要敲 `pk`，pkexec 清环境。
3. 分层：`pk` 决定能不能跑，auditd 记录实际跑了什么，AppArmor 限制最多能造成多大损失。
4. 最终方向：特权服务 + 命名操作（如 `openclaw.restart`），手机上显示人话。审一行任意
   命令很难读懂；`pk` 只做无法预先列出的回落，日志里反复出现的命令升级成命名操作。

## 分级 L0–L3

谁发起不重要，按动作风险定级；级别由固定规则（命令模式、能力目录）决定，不让 LLM 判定。

| 级别 | 例子 | 审批 |
|---|---|---|
| L0 只读诊断 | `herd status`、`nft list`、读日志 | 自动放行，只记日志（可放 sudoers） |
| L1 范围内例行操作 | `herd restart openclaw`、本 workshop 的 podman | 第一次点，之后在租约内同模式自动放行 |
| L2 有状态可回滚 | 改配置后 reconfigure、`herd stop` | 每次点，卡片显示摘要和回滚方式 |
| L3 不可逆/涉秘/新增能力 | sops 解密、删数据、改网络、任意脚本 | 看完整内容后生物识别确认，不能批量 |

- 请求卡片把事实（请求者、命令或脚本 hash、涉及文件、diff、能否回滚）和 agent 自述的
  理由分开。
- 审计：每个 workspace 一份 append-only 日志加每日摘要。
- 重复点击 = 能力目录的 bug 报告。
- 人和 agent 唯一的区别：人的 sudo 批准后自己执行；agent 的命令由宿主侧执行器执行，
  agent 拿不到 root。
- 第一版可以不做租约：L0 免密，其余全部手机确认并显示命令。

## 能力目录

- 命名能力包：`gpu-probe`、`maa-adb`、`podman`、`ssh:<host>`，各自展开成挂载、设备、
  ssh key 等。每个能力 PR 审一次，之后只决定“给它 `ssh:<host>`”。
- session 声明 `capabilities: [paseo-api, podman, ssh:<host>]`，不再写路径级策略。
- `ssh:<host>` 展开为 ssh-agent 里的一把 key，加上网桥上对 `<host>:22` 的一条转发例外。
- 运行时授权不 reconfigure：ssh-agent 白名单和 known_hosts 热加载，新主机指纹由宿主侧
  `ssh-keyscan` 取得。
