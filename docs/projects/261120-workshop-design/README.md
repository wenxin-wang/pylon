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
- 11/20：约定做成 workshop skill，由 `AGENTS.md` 要求先读；里面包括模块和级别的概念、
  wiki、读 space、agent 的记忆、「决定」的写法、项目 skill 和全局 skill 的分工。
- 11/21：全局 skill 可以有多个来源，现在只有策略仓库一个；workshop skill 也放在那里，
  不公开。pylon 自己不放 skill，也就没有 workshop 这个模块。
- 11/20：人的偏好放进策略仓库，见 agent-policy 项目。
- 11/20：全局 skill 从每个来源只跟 `main` 的检出装，不经 Guix；模块 skills（能管），
  见 [docs/skills.md](../../skills.md)。备选是单独一个 Guix profile（快、能回退、
  容器共享 store 时直接可见），容器需要时再看。

## 文档

- [TODO](TODO.md)：进度。
