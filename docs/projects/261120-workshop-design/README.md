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

- 11/20：先做 workshop 本身的设计，uraj-migration 挂起。
- 11/20：不做派活的工具和信箱。agent 以某个 workshop 为默认工作目录，可以改自己的
  workshop、关联的仓库，以及别的 workshop，暂不限制范围。agent 之间怎么合作以后摸索，
  很快会单独讨论。
- 11/20：通用约定做成 workshop skill，每个 workshop 的 `AGENTS.md` 只写必须时刻成立的
  规则和自己的内容。11/21：全局 skill 和人的偏好都放在策略仓库，不公开；
  见 [docs/skills.md](../../skills.md) 和 agent-policy 项目。
- 11/21：contained-agent v2 上线前不拆：现在的容器读不到全局 skill，约定以 pylon 的
  `AGENTS.md` 为准，策略仓库里的 workshop skill 是副本。
- 11/20：wiki、读 space、agent 的记忆、「决定」的写法：已写进 `AGENTS.md`。

## 文档

- [TODO](TODO.md)：进度。
