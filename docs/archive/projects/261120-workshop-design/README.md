# workshop-design

## 目标

定下 workshop 这套约定本身：pylon 之外的 workshop 怎么沿用它，workshop 和 space
怎么分工，agent 关心的知识放在哪。定完之后，人能说清一个仓库要成为 workshop
需要满足什么，以及一份信息该写到哪里。

## 背景

11/19 定了：space 只有人写，agent 最多只读；agent 主导的工作放进 workshop。
pylon 是第一个 workshop，它的 `AGENTS.md` 是目前唯一一份约定，但写的时候只考虑了
pylon 自己（公开、有代码、要迁 uraj）。以后会有别的 workshop，包括私有的、没有代码的。

之前 agent 的知识库放在一个 space 下面、由 agent 写，和“space 只有人写”冲突，
要搬进 workshop。之前设计过的信箱和投递工具已经作废，太复杂。

uraj-migration 先挂起：迁过来的模块要放进这里定下的约定里。

## 待定

暂无。

## 决定

11/21 项目结束，决定都已有去处：

- 先做设计、uraj-migration 挂起：设计已完成，uraj-migration 的状态随之改为等人。
- 不做派活工具和信箱、agent 在哪干活：workshop skill 的 “Where agents work” 一节。
- 约定做成 workshop skill，内容和「决定」的写法：workshop skill 本身；pylon 的
  `AGENTS.md` 要求先读它。
- 全局 skill 的来源、装法、不经 Guix 的理由：[docs/skills.md](../../../skills.md)。
- 人的偏好和策略仓库：[agent-policy](../../../projects/261120-agent-policy/README.md)。

## 文档

- [TODO](TODO.md)：进度。
