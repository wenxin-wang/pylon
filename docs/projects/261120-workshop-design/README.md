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

由人来定。agent 列出选项、代价和建议：

- **约定能不能浓缩成一个 skill。** skill 是一个带说明的目录（`SKILL.md`），
  Claude Code 和 Codex 都支持：只有名字和一句描述常驻上下文，相关时才读全文。
  好处是一份约定所有 workshop 共用，不用每个仓库复制；workshop 不用含有约定本身。
  代价：
  - skill 是按需加载的，agent 可能没读就动手。“不提交到 `main`、不提交秘密、
    不部署”这类必须时刻成立的规则不能只放在 skill 里。
  - skill 要装到每台机器、每个容器里（以后 contained-agent-v2 的容器也要挂进去），
    装法本身是一个模块。
  - 改约定就是改 skill，所有 workshop 同时生效，好坏都是。

  方案：
  A：全部写进 skill，每个 workshop 的 `AGENTS.md` 只有一行“先读 workshop skill”；
  B：skill 放完整约定（文档布局、子项目、PR 写法、wiki），每个 workshop 的
  `AGENTS.md` 写必须时刻成立的几条、这个 workshop 自己的东西（能读哪些 space、
  有代码的话模块表和级别），再指向 skill；
  C：不用 skill，各 workshop 复制 `AGENTS.md`。
  建议 B。skill 的源文件放在 pylon，这是新目录和新模块（级别“能讲”，它就是规则
  本身），开工前要人点头。
- **wiki 的规则。** 每个 workshop 可以有一个 `wiki/`，放 agent 关心、人不关心的
  知识和素材。要定：
  - 进不进 git。pylon 是公开的，进 git 就是公开；不进 git 就会丢，也不能跨机器。
    选项：A：进 git，公开 workshop 的 wiki 只放能公开的；B：`wiki/` 单独一个私有仓库，
    主仓库忽略它（之前就是这样）。建议 A，简单；私有素材放私有 workshop。
  - 和“人始终能看懂”怎么相处。人不读 wiki，所以要有一条：`src`、`env` 和活文档
    不依赖 wiki；wiki 里得出的结论要用到，就写进活文档或设计文档，经 PR 给人看。
  - 结构先不定，用起来再说（之前想的是素材、页面、索引、日志）。
- **workshop 能读哪些 space。** 以后很可能由 contained-agent-v2 决定：容器里只挂
  一个或一组 workshop 及其关联仓库允许访问的目录，规则只是过渡。过渡期的选项：
  A：不读，需要时由人摘出来交给 session；B：可以读，规则禁止引用；
  C：默认只读 `AGENTS.md` 列出的，人在某个 session 里明说时可以多读（11/20 就是这样）。
  建议 C。
- **agent 的记忆。** 各工具自带的 memory 是私有的，别的 agent 看不到。建议：进度只写
  `TODO.md`，agent 关心的知识进 `wiki/`，工具自带的 memory 只放人的偏好和工作方式。

## 决定

- 11/20：先做 workshop 本身的设计，uraj-migration 挂起。
- 11/20：不做派活的工具和信箱。agent 以某个 workshop 为默认工作目录，可以改自己的
  workshop、关联的仓库，以及别的 workshop，暂不限制范围。agent 之间怎么合作以后摸索，
  很快会单独讨论。
- 11/20：每个 workshop 可以有 `wiki/`，放 agent 关心、人不关心的知识和素材；
  不再有独立的 llm-wiki。

## 文档

- [TODO](TODO.md)：进度。
