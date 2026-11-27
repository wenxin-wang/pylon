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

[uraj-v2]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/README.md
[box-prototype]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-prototype-design-20261115.md
[zombie-debug]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/pasta-zombie-debug-20261115.md
[network-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/box-network-measure-20261115.md
[grants-measure]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/grants-measure-20261115.md
