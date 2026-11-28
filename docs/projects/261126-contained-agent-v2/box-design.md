# 盒子的设计

盒子是 agent 工作的地方：一个长期存在的容器，coding agent 和它用的 Podman 都在里面，
每个盒子对应一件事。本文按节从 uraj 的 [v2 README][uraj-v2] 迁过来，边迁边讨论；
还没迁的节暂见那里。

## 盒子怎么起

盒子由盒子管理服务 `box-manager` 直接调 rootless Podman 起（11/27），rootfs 换成
Guix 的：一个几乎空的目录，只读挂 `/gnu/store`，软件全部来自一个 Guix profile，不用
Fedora 镜像。原型和实测见 uraj 的
[box-prototype-design-20261115][box-prototype]：嵌套 Podman 跑得起 Postgres 这类切换
UID 的镜像，盒子里访问发布的端口是透明的。

为什么不直接用 Guix 容器当盒子：

- 非特权的 `guix shell --container` 和 `guix system container` 只往 `uid_map` 写自己
  一个 UID，盒子里的 Podman 拿不到 subordinate ID，切换 UID 的镜像跑不起来。
- 补上 `newuidmap` 的启动器只能做成主机侧的长期服务（会话里用不了 subordinate ID，
  会话结束还会杀掉盒子）。再加上 exec 和 stdio、cgroup 委托、pasta、空闲回收，就是在
  重写 rootless Podman 的外层。
- `guix system container` 以 root 运行时，盒子里就是真 root，太宽。

原型里定下的做法：

- `newuidmap`、`newgidmap` 要 file capability，setuid 副本写 `uid_map` 报 EPERM。
  `/gnu/store` 里的文件不能带特权位，所以启动时由盒子里的 root 复制到 tmpfs 再
  `setcap`，不需要主机 root。
- 盒子的 PID 1 要用会回收孤儿的 init（`podman run --init` 或盒子里的主管进程）：Podman
  起的 pasta 会 fork 到后台成为孤儿，不回收就留成 zombie，见 uraj 的
  [pasta-zombie-debug-20261115][zombie-debug]。
- 盒子里有主机的 Guix daemon，agent 在盒子里装包、重新加载 manifest 不用申请授权。
  代价：Guix daemon 在主机上下载，绕过盒子的网络限制；接受这个风险，不给盒子单独做
  store。
- 每个盒子默认有 Podman（11/27）：盒子有一段 subordinate ID、放宽的 seccomp 和
  `/dev/fuse`。

还没做：盒子的 seccomp 基础规则仍从 Fedora 镜像取，要换来源；盒子 rootfs 里的
`/etc/subuid`、`/etc/passwd` 等要归盒子里的 root，原型里归 agent 自己的 UID。

## 网络

- 盒子不能访问主机回环，也不能访问内网；公网和 Paseo relay 放行。
- 做法：pasta，加盒子 netns 里的 nftables。规则由盒子管理服务设，运行中可改；不换
  bridge。

为什么：

- 默认 pasta 下，盒子连不到主机上只监听回环的服务，包括主机的 Paseo daemon；但主机上
  监听所有地址的服务经 `169.254.1.2` 能连到，局域网也通（uraj 的
  [box-network-measure-20261115][network-measure]）。所以只靠 pasta 不够。
- pasta 本身不能按目的地址过滤，原来以为要换 bridge 加 root 侧接 veth。实测
  （uraj 的 [grants-measure-20261115][grants-measure]）发现主机用户不用 root 就能进盒子的 netns
  设 nftables，规则生效后主机端口连不上、公网和 relay 照通，盒子里的 agent 改不了规则。
  所以留在 pasta。

还没测：内网网段的规则（测试目标本来就不响应，规则写法和 `169.254.1.2` 那条一样）；
嵌套容器经盒子里的 pasta 出网是否同样受这些规则约束（按理是）。正式实现对 TCP 用
`reject with tcp reset`，让程序立刻失败，而不是等超时。

## 文件怎么进盒子

- 仓库默认在盒子里 clone（11/27）：盒子开始时的那个 workshop 在建盒子时 clone，运行中
  加的仓库由盒子里的 agent 用 gh 现场 clone，两者是同一种方式。
- 超大的、要用自己的工具管理的仓库（例如一组 monorepo）另想办法，接入时再定，默认的
  clone 不套用到它们身上。候选之一是 reflink 拷贝主机上的 checkout：uraj 实测拷 uraj
  用 0.04 s、btrfs 上新占用为 0，拷完第一次 `git status` 要 1 s 左右（stat 缓存失效），
  大仓库没测（uraj 的 [box-network-measure-20261115][network-measure]）。
- 盒子里的 `.git` 可写。主机不在盒子里的仓库上运行 git，盒子和外界只经远端 push、fetch
  交换。
- 路径沿用主机的 layout：仓库放在和主机相同的绝对路径，HOME 也是同一路径。HOME 是合成
  的，只放声明过的东西。
- worktree 用盒子里 Paseo 建的（11/27），一个 session 一个；删盒子时一起删。

为什么：

- 默认 clone，不默认 reflink：只有一种进盒子的方式；盒子不依赖主机上的 checkout，主机
  本地的 `out/`、`tmp/`、`.git/config` 不会带进去，`box-manager` 也不用读主机上的仓库。
  代价：主机上没 push 的提交和没提交的改动带不进去，要先 push；私有仓库要等 agent 的
  GitHub 身份就位才能 clone，不过盒子要 push 本来就要这个身份。
- 用 Paseo 的 worktree，少维护一套逻辑和代码。uraj 原来定的是 agent 自己建、不放在
  Paseo 的 worktree 根目录下，理由没有记下来；推测是当时还在比较路线 A，daemon 留在
  主机上，用它建 worktree 就成了主机在盒子的仓库上跑 git。路线 C 下盒子 daemon 的
  worktree 根目录在盒子里，这条理由不成立了。代价：worktree 在盒子 daemon 的
  `$PASEO_HOME` 下面，重建盒子要连它一起保留；`$PASEO_HOME` 本来就要保留 `server-id`。
  以后换掉 Paseo 时再重新考虑。

### 共享路径

有的目录不进盒子的仓库副本，直接把主机上的原目录挂到盒子里的同一路径，例如
traj-record 的 `instances`、`out`：

- 共享路径是盒子策略里的一类授权，写在 agent-policy 里。仓库里可以放一份默认清单，但
  只是建议。
- 下面不能有 `.git`，挂载前检查。
- 运行中追加，和其他授权一样改盒子策略，见“运行中改授权”。
- 多个盒子挂同一份时，要按 session 分子目录。

## 访问盒子里的服务

- 按需转发：agent 要让主机访问盒子里的某个端口时，请盒子管理服务开一个转发，接到主机的
  `127.0.0.1:<端口>`。端口号尽量和盒子里相同，冲突时另分一个。
- 只开在主机回环上，不算授权，agent 可以自己开，不用确认。
- 做法（11/27）：能用 pesto 就用 pesto，不行就用 socat。
  - pesto：passt 带的客户端，能在运行中给 pasta 增删端口转发。`box-manager` 起盒子时给
    盒子的 pasta 传 `-c <控制 socket>`，开转发就是 `pesto -A -t 127.0.0.1/<端口>`，
    关掉用 `-D`。盒子里和主机上都不用多跑进程。
  - socat：盒子里用 socat 把端口转成 `/run/box/` 下的 unix socket，和路线 C 暴露
    `paseo.sock` 一样；`box-manager` 在主机上把它接到回环端口。代价：每个转发在盒子里有
    一个 socat 进程，`box-manager` 要为每个端口跑一个回环 TCP 到 unix socket 的转接。
- 以后再做：按 `*.localhost` 分发 HTTP 的反向代理；从别的机器访问。

为什么：

- 建盒子时用 `-p` 发布、或者 pasta 的 `-t auto`，都不采用：运行中加不了，多个盒子的端口
  会冲突。
- pesto 从 passt 2026_05_07 起有（[Fedora 的更新说明][fedora-passt]、
  [pesto(1)][pesto-man]）。主机系统固定的 Guix（uraj 的 `env/guix/channels-lock.scm`，
  10/21 的提交）里 passt 还是 2024_12_11，没有 pesto；Guix 上游 10/24 升到了
  2026_10_02。`box-manager` 用的 Podman 和 passt 可以来自另一个 channel 版本的 profile，
  不用等主机系统升级。

还没测：Podman 的 `--network pasta:...` 能不能把 `-c` 传给 pasta。Podman 自己用 pesto
的地方是 rootless bridge 下的实验选项 `rootless_port_forwarder`
（[podman#28478][podman-pesto]），不是 pasta 模式。第 3 步做原型时测；测不通就用 socat。

## 盒子策略

- 放在 agent-policy 的 `boxes/<盒子名>.toml`，一个盒子一个文件（11/28）。没有文件的盒子
  只有默认授权。
- 写的是这个盒子的授权：SSH 目标、GitHub 令牌的范围、设备、网络放行、共享路径等，具体
  的键迁到各节时定。设备只写 `gpu` 这类名字，实际路径由 `box-manager` 在主机上查。暂不做
  共用授权，每个盒子把授权直接写在自己的文件里，真要复用时再加一种共用文件。
- 格式是 TOML，用 Python 标准库的 `tomllib` 读。严格校验：未知的键、类型不对都报错。
  校验代码在 pylon，主机上和 CI 里用的是同一份。

### 生效前检查

agent-policy 的新提交要过两道检查才在主机上生效。做在 `bin/pylon-skills update` 里，
它本来就在维护 agent-policy 那份只跟 `main` 的检出：

1. fetch 之后，取本地检出到 `origin/main` 之间的提交。
2. 查批准：用 GitHub 的 repository activity API（`GET /repos/{owner}/{repo}/activity`）
   列出这段时间里 `main` 的每次变动（`before`、`after`、操作者、类型），要首尾相接覆盖
   整段，操作者都是仓库主人。
3. 查格式：用盒子策略的校验检查 `origin/main` 上的 `boxes/`。
4. 两道都过才快进，打出哪些盒子的策略变了，再让 `box-manager` 对运行中的盒子对账（见
   “运行中改授权”）；有一道不过，就保留上一版并报错。

触发（11/28）：每台主机上一个 Shepherd 定时器，每分钟用 `git ls-remote` 查一次 `main` 的
提交号，变了才跑完整的 `update`；没变就只让 `box-manager` 对账，它发现提交没变就什么都不做。
连不上网只记一行日志，下一分钟再试，网络恢复后自动补上。

`box-manager` 只读这份检出，不读人的工作副本（里面可能有没合并的改动）。它读策略时再校验
一遍兜底：某个文件不合格，只拒绝那个盒子新的授权操作，已经生效的不动。

agent-policy 里另有一个 CI workflow：checkout 公开的 pylon，跑同一份校验，PR 上显示
✓ 或 ✗。它拦不住合并，人看到 ✗ 就不合。

批准这一道等 agent 换成 GitHub App 身份后才有用：在那之前 agent 用人的 token，操作者
永远是人。所以和“agent 的 GitHub 身份”一节一起做。

为什么：

- 一个盒子一个文件：PR 的 diff 看文件名就知道动的是哪个盒子，`boxes/` 目录本身就是所有
  盒子权限的汇总，不用另外生成。
- TOML：标准库能读，不加依赖，也不用手写解析器；能写注释，`no` 这类字符串不会被当成布尔
  值。JSON 不能写注释；YAML 标准库里没有。
- 校验在 pylon，不在 agent-policy：策略里每个键都要 `box-manager` 去执行，加一个新授权，
  pylon 本来就要先改；只有一份规则，CI、主机上的检查和 `box-manager` 才不会对不上。
- 检查放进 `pylon-skills update`，不另写一个同步程序：偏好和 skill 带的 hook 也用人的
  权限跑，本来就该过同一道检查。
- 用 activity API：agent-policy 是免费账号下的私有仓库，用不了 ruleset 和分支保护（11/28
  读 ruleset 接口，GitHub 回答要升级 Pro）；activity API 在这个仓库上能用（11/27 试过，
  最近 5 次都是人的 `pr_merge`）。App 合并 PR、直接推 `main`、force push，操作者都是
  App 的 bot，都会被拦下。
- 格式也在每台主机上查，不只靠 CI：写错的策略到不了主机，上一版保留着，盒子不会突然
  用不了。
- 定时查，不用推送：定时器本身没有任何权限，检查都在 `update` 里，所以怎么触发都不影响
  安全，选最简单的。GitHub webhook 要主机能被访问到；经 Paseo relay 叫醒主机上的监听程序
  能做，但要写 relay 客户端、借用 Paseo 的公共 relay，断网时发来的消息还会丢，最后仍要定时
  查兜底。self-hosted runner 不行：同仓库的 PR 也会在 runner 上跑 PR 分支里的 workflow，
  等于让 agent 用人的权限在主机上执行代码。

代价：

- pylon 要先支持新的键，策略才能用；CI 用 pylon `main` 上的版本，主机上用本机装的那份。
- skills 模块要调用盒子策略的校验代码，这段代码至少是“能管”级；`box-manager` 大概率是
  “能讲”级，所以实际不抬级别。skills 模块多调一个 GitHub API，`update` 还会改运行中盒子的
  授权，做的时候更新它的影响范围。
- agent 的 PR 能改 CI workflow，让检查永远通过，只能靠人读 diff 看出来；主机上的检查跑的
  是本机的 pylon，不受影响。
- 合并就是唯一的关口：人不再自己跑 `update`，偏好、skill、hook 和盒子策略合并后一分钟内
  就到了所有主机上。
- 检查失败只记在日志里，有了通知通道才会推给人。
- 每台主机的 Guix Home 要加一个定时服务。
- 没核实：activity 记录保留多久，文档没写。查不到时，人可以在检出里手动快进来放行。

以后：

- 生效时推送通知，等有了通知通道再做。
- 有 pylon 不读、只属于 agent-policy 的检查（例如偏好或 skill 的写法）时，让 `update` 在
  批准通过后顺带跑 agent-policy 里的检查脚本。
- 同一流程以后可以接主机上的自动部署（Guix、agent-policy）：拉 `main`、查批准、跑检查，
  最后一步从快进检出换成部署。

## 运行中改授权

- 盒子运行中加、撤授权，只有一条路：改盒子策略的 PR，人合并（11/28）。不另设请求和确认
  卡片。
- 生效：生效前检查通过、快进之后，`pylon-skills update` 让 `box-manager` 拿新策略和每个
  运行中的盒子对账，缺的加上，多的撤掉（11/28）。`box-manager` 没在跑就跳过；没运行的盒子
  下次启动时按当时的策略起。
- 做法（uraj 11/15 定）：`box-manager` 以普通用户身份完成，不用 root。
  - 挂载：先进 rootless Podman 的 user 和 mount namespace（这里看得到主机路径），用
    `open_tree(OPEN_TREE_CLONE | AT_RECURSIVE)` 克隆主机目录，再进盒子的 user 和 mount
    namespace，`move_mount` 挂到目标路径；撤销用 `umount2(MNT_DETACH)`。
  - 网络：进盒子的 netns 改 nftables。
  - 凭据：走 socket，每次调用时查当前策略，策略一变就生效，不用对账。
- 撤销（11/28）：卸载、删规则立刻做，但已经打开的文件描述符收不回。`box-manager` 报告哪个
  盒子撤了什么、要彻底收回得重建盒子；重不重建、什么时候重建，由人定。
- 要 root 的操作（sudo、部署）不归 `box-manager`，交给另一个能以 root 执行的服务；pylon 里
  还没有设计。

为什么：

- 只走 PR：策略在 agent-policy，人合并 PR 就是批准，再加一张卡片是重复的。
- 由生效前检查的最后一步触发对账：主机上“拉 `main`、查、生效”是同一个流程，以后接自动
  部署也是在这一步加东西。
- 不自动重建盒子：重建会打断盒子里正在跑的 agent。
- 不用 root 就够：uraj 实测（[grants-measure-20261115][grants-measure]），主机用户在
  运行中的盒子里改 nftables、增减挂载都成功；盒子里的 agent 没有特权，`nft`、卸载挂载、
  自己 bind mount 都失败，进不了盒子的 netns。

代价：

- 慢：要等 PR 合并，再等最多一分钟的定时器。临时用一下的授权要提两个 PR，一个加、一个撤。
  以后有了审批通道，可以再加限时的临时授权。
- skills 模块要知道 `box-manager`。
- 撤销收不干净，直到重建盒子。设备也一样，迁“设备”一节时细说。

还没测：uraj 实测时盒子由 `contained-podman` 起，进的是它的 user namespace；改成
`box-manager` 自己调 Podman 后，入口换成 rootless Podman 自己的 user namespace，按理是
同一个，第 3 步做原型时测。

[uraj-v2]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/README.md
[box-prototype]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-prototype-design-20261115.md
[zombie-debug]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/pasta-zombie-debug-20261115.md
[network-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-network-measure-20261115.md
[grants-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/grants-measure-20261115.md
[fedora-passt]: https://bodhi.fedoraproject.org/updates/FEDORA-2026-a9cb0876bc
[pesto-man]: https://www.mankier.com/1/pesto
[podman-pesto]: https://github.com/podman-container-tools/podman/pull/28478
