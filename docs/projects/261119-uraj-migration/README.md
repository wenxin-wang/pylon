# uraj-migration

## 目标

uraj 的模块一个一个迁到 pylon，迁完 uraj 退役。迁完之后，pylon 模块表上的每一行
人都能讲出来；每个模块懂到它的级别要求的程度。

## 背景

uraj 从 2026-10 起由 agent 开发，一个半月 250 多个提交、约 1.8 万行，用了 C、
Guile、Guix、Python 四种语言，人已经看不懂了。原因不是缺了哪一步流程，而是产出
远快于人能读的速度，审 PR 时只看到 diff，看不到整体。

所以迁移不是整体搬家，而是按模块重新梳理：先在模块表上登记这一行，定级别，能简化的
顺手简化，用不上的留在 uraj 跟着退役。投入按级别分：

- 能讲：和人一起逐个重新梳理设计。
- 能管：agent 迁移，附一份影响范围说明，人只看这一份。
- 能用：agent 批量迁移，人只确认没有更高级别的模块依赖它。

过渡期 pylon 和 uraj 是两个 Guix channel，部署仍然走 uraj，直到对应部分迁完。

## 待定

- **各模块的级别。** agent 的猜测，请改：
  - 能讲：contained-agent、restricted-network、secrets。它们是“人能不能信任
    agent”的边界。
  - 能管：paseo、guix（主机和部署）、openclaw、new-api。
  - 能用：maa、desktop、dotfiles、playbooks、coding-style、config。
  - emacs：天天用，可能是能管。
  - 级别顺着依赖传：例如 `container/seccomp.py` 被 contained-agent 用，就是能讲，
    即使 openclaw 也用它。
- **第一个迁哪个。** 建议 contained-agent-v2：它的设计要在 pylon 里做，没有历史包袱，
  而且是能讲级，正好用来检验这套流程。代价是 v2 还在设计中，迁移和设计同时进行；另一个选项是
  先迁一个小的能用级模块（例如 config）试流程，风险小，但学不到什么。
- **pylon 什么时候成为 Guix channel、怎么和 uraj 互相依赖。** 等第一个模块迁进来再定。

## 决定

- 11/19：pylon 接替 uraj；首要目标是人看得懂。
- 11/19：理解分三级（能用、能管、能讲），级别顺着依赖往下传；共用代码不复制。
- 11/19：主力语言 C/C++、Python；Guile 只用在 Guix 必须的地方，保持能看懂；
  新语言要人点头。
- 11/22：uraj 进入维护，不再加新功能；contained-agent-v2 的设计在 pylon 里做。

## 文档

- [TODO](TODO.md)：进度。
