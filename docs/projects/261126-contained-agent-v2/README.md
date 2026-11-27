# contained-agent-v2

## 目标

agent 在盒子里工作。盒子是一个长期存在的容器，coding agent 和它用的 Podman 都在
里面，每个盒子对应一件事。权限只描述盒子的边界（挂载、网络、设备、凭据 socket）；
agent 看到的是一台普通开发机，不需要知道 Podman 运行时在哪、端口怎么转。

v2 做好之前，uraj 里的 contained-agent（v1）照常用，不为 v2 改动。

做 v2 时还要做到（来自其他项目，做完一条就在这里和来源项目里都划掉）：

- Codex 的项目信任（或整个 `~/.codex`）在盒子重建后还在，再新开 Codex session 验证
  pylon 的 `.codex/config.toml`：开工和压缩后都拿到 llm-wiki 索引。v1 退出时丢掉配置
  修改，项目没被信任，hook 不加载。
- 能由定时器启动盒子里的 session，跑反思（[reflection](../261122-reflection/README.md)
  第 4 步）。
- 可读写地挂载宿主的 `~/.local/state/agents/`，session 笔记才能在盒子重建后留下来
  （[reflection](../261122-reflection/README.md) 第 3 步）。
- 调研怎样让 agent 的环境只拿到它该用的秘密。把某一个秘密单独移出环境，一次能做到；
  但秘密不都天然分出级别，要找通用的办法。
- 让 agent 写的脚本默认用一个有额度的 API key（也可以在 v2 做完之后）：Claude Code
  自己的 key 改由 `apiKeyHelper` 读，不放进环境变量，Bash 里只看得到有额度的那个；
  额度用完由人去加。先验证两个 key 同时在时 Claude Code 用哪个，再定变量名；Codex 若走
  同一个网关，一起处理。agent-policy 同时加一句：额度用完就报告花费并停下，不找别的 key
  绕过去。起因：11/24 的重放脚本从环境变量拿到了 Claude Code 自己那个不限额的 key。
- 权限策略放进 agent-policy，agent 用 GitHub App 的身份，同步时校验批准
  （[agent-policy](../261120-agent-policy/README.md) 第 2 步）。
- 做到审批通道时，更新 `llm-wiki/sudo-approval.md`，它停在 11/02 的早期调研。

## 背景

v1 里，agent 容器和 [Podman 运行时][podman-guide]是主机上并列的两个容器，靠一个
API socket 相连：

- 网络不通：运行时里 `-p` 发布的端口和 `podman inspect` 给的 IP 都在运行时的网络
  namespace 里，agent 访问 `127.0.0.1:PORT` 不通。
- `/tmp`、bind 源路径要两边对齐，挂载策略写两份。
- 拆开的原因是 Paseo broker 的 user namespace 只映射用户自己的 ID，agent 容器里起不了
  嵌套 Podman，见 podman-guide 的 Host Podman service 一节。

另外，v1 的 agent 和 Paseo daemon 共享网络；daemon 没设密码时 HTTP API 不鉴权，能建
agent、开终端、回答任意 agent 的权限请求。这个问题由 v2 解决，不单独抢修。

v2 的设计 11/15 到 11/17 在 uraj 里讨论过，做了原型和实测（见“文档”一节）；11/26 起
在这里接着做，设计按节迁进 [box-design](box-design.md)。还没迁的部分暂见 uraj 的
[v2 README][uraj-v2]。

## 待定

- **workshop skill 什么时候改。** 一个盒子一个 workshop、盒子之间写信（见决定）定了以后，
  workshop skill 的 “Where agents work” 要改：它现在写的是“可以改别的 workshop，范围暂
  不限”“How agents work together is still open”。建议等 v2 能用了再改，因为在那之前
  agent 还不在盒子里，改了就和实际不符。
- **运行中加的仓库要不要人确认。** 盒子里的 agent 用 gh 现场 clone 别的仓库，GitHub App
  的令牌就得覆盖这些仓库：盒子策略里事先列出能访问哪些仓库，还是 agent 运行中申请、人确认。
  迁到 agent 的 GitHub 身份那一节时定。
- **盒子策略在 agent-policy 里怎么写**：每个盒子一个文件还是别的分法，要不要 CI。迁到
  盒子策略那一节时定。
- uraj 里还开着的几条（盒子用容器还是 microVM、部署接口放不放进本项目、创建 workspace
  的卡片和上游 Paseo 的修改），迁到相应的节时搬过来。

## 决定

- 11/15–11/17 在 uraj 定的设计，凡没被下面推翻的都还有效：已迁的见
  [box-design](box-design.md)，没迁的暂见 uraj 的 [v2 README][uraj-v2]。
- 11/26：盒子是独立的概念，和 space、area 无关，每个盒子对应一件事；11/16 按 area 分
  盒子、策略写在 area 文档里、workspace 对应 space 里 project 的决定都作废。
- 11/26：盒子策略放在策略仓库 agent-policy；agent 的 GitHub 身份用 GitHub App。见
  [agent-policy](../261120-agent-policy/README.md) 的决定。
- 11/26：一个盒子开始时只放一个 workshop，限定工作范围。别的仓库运行中按需加：盒子里有
  Paseo 和 gh，一个任务要哪些仓库，人一开始也想不清。agent-policy 要改时，在盒子里现场
  clone，或者给它提 issue。
- 11/26：盒子之间经 GitHub issue 通信，像写信，延迟高：agent 起草，人审阅后自己发。
  发出去发现不妥，就删掉。
- 11/26：单位盒子的策略（例如能 SSH 到哪些工作主机）也放进 agent-policy。
- 11/26：设计按节迁，一节一个 PR；uraj 里的记录和 `exp/` 脚本留在 uraj，用固定 commit
  的链接引用。
- 11/27：每个盒子默认有 Podman，agent 测试方便，也只有一种盒子要测。代价：每个盒子都有
  一段 subordinate ID、放宽的 seccomp（允许 `mount`、`unshare`、`setns` 等）和
  `/dev/fuse`，agent 能在盒子里建 user namespace，碰到的内核接口更多。
- 11/27：盒子由盒子管理服务自己调 rootless `podman` 起，服务叫 `box-manager`，不再要
  单独的 Podman 服务（uraj 原型借用的是 v1 的 `contained-podman`：一个跑
  `podman system service` 的用户级 Shepherd 服务，v1 要它是因为 Paseo broker 的
  user namespace 里用不了 subordinate ID）。少一个服务和一个 socket。没测过：运行中加
  挂载时进入的 user namespace，换成 `box-manager` 自己调 `podman` 后应该还是同一个
  （rootless Podman 的容器共用一个）。
- 11/27：盒子里的 session 不经 Paseo broker（v1 里主机 daemon 经它在主机上用
  contained-agent 起 agent）：盒子里的 daemon 直接起 agent，盒子本身就是沙箱。broker
  在 v2 稳定后和 v1 一起退役，不迁进 pylon；它的共用 SSH agent 在迁 SSH 那一节时另定。
- 11/27：v1 退役后不留 host 层（agent 直接跑在主机上的那一档）：主机的 Paseo daemon 不配
  provider，只连各盒子。以后有要主机权限的需求，起一个带特权的盒子；现在还没有这种需求。
- 11/27：v1 退役后，主机的 Paseo daemon 仍关在 Guix 容器里，只挂 `~/.paseo` 和放盒子
  socket 的目录。它不起 agent、不放项目，经 unix socket 连各盒子的 daemon；登记盒子是
  盒子管理服务调 `paseo host add`，daemon 不用反过来找盒子管理服务。还关着，是因为它
  仍能在主机上开终端、跑 git（在 app 里随便打开一个主机目录就行），经 relay 配对的 app
  等于拿到主机上人的 shell。代价：沿用 v1 的容器服务。维护嫌麻烦时可以改成不关，代价是
  app 和 daemon 自身的缺陷都直通主机。

## 文档

- [TODO](TODO.md)：进度。
- [step-1-handoff](step-1-handoff.md)：第 1 步交接：迁设计并讨论，已知的坑。
- [box-design](box-design.md)：盒子的设计和理由。
- uraj 里的记录（11/15，写完不改）：
  - [box-prototype-design-20261115][box-prototype]：Guix rootfs 的盒子原型方案和实测。
  - [paseo-route-design-20261115][route-design]：Paseo 接盒子的路线 A、B、C 调研和建议。
  - [box-network-measure-20261115][network-measure]：盒子的网络可达性和 reflink 代价。
  - [grants-measure-20261115][grants-measure]：不用 root 在运行中的盒子里改 nftables、
    增减挂载，agent 绕不过去。
  - [devices-measure-20261115][devices-measure]：运行中给盒子加、撤 GPU render node 和
    `/dev/kvm`。
  - [pasta-zombie-debug-20261115][zombie-debug]：运行时留下 zombie pasta 的原因。
  - [route-c-measure-20261115][route-c-measure]：路线 C 原型实测。
  - Paseo 0.15.4 源码调研，路线调研的依据：[protocol][protocol-review]（app 和 daemon
    之间的消息）、[relay][relay-review]（身份、配对、加密、推送）、
    [host-exec][host-exec-review]（daemon 在主机上执行什么）、
    [connected-hosts][connected-hosts-review]（发现、权限、隔离和缺口）。

[uraj-v2]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/README.md
[podman-guide]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/podman-guide.md
[box-prototype]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-prototype-design-20261115.md
[route-design]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/paseo-route-design-20261115.md
[network-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-network-measure-20261115.md
[grants-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/grants-measure-20261115.md
[devices-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/devices-measure-20261115.md
[zombie-debug]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/pasta-zombie-debug-20261115.md
[route-c-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/route-c-measure-20261115.md
[protocol-review]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/paseo-protocol-review-20261115.md
[relay-review]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/paseo-relay-review-20261115.md
[host-exec-review]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/paseo-host-exec-review-20261115.md
[connected-hosts-review]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/paseo-connected-hosts-review-20261115.md
