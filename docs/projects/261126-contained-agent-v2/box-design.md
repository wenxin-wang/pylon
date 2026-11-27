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
- 运行中追加，走和其他运行中授权一样的挂载方式和确认。
- 多个盒子挂同一份时，要按 session 分子目录。

[uraj-v2]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/README.md
[box-prototype]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-prototype-design-20261115.md
[zombie-debug]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/pasta-zombie-debug-20261115.md
[network-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-network-measure-20261115.md
[grants-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/grants-measure-20261115.md
