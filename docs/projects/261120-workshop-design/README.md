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

- **约定放在哪、怎么保证 agent 读到。** 目标：改一处，所有 workshop 生效。
  每个 workshop 的 `AGENTS.md` 只写必须时刻成立的几条、这个 workshop 自己的东西
  （能读哪些 space，有代码的话还有模块表），再加一句“开工前先读完整约定”。
  完整约定（文档布局、子项目、PR 写法、wiki）放一处。
  “一定要读”只是 `AGENTS.md` 里的一条规则，和其他规则一样靠 agent 遵守，
  不是 skill 的加载机制保证的。

  放法：
  A：做成 skill，源文件在 pylon，链接到各工具的 skill 目录。`AGENTS.md` 写“先读
  workshop skill 的 `SKILL.md`”。这只是把按需加载变成每次都读，不违背 skill 的用法；
  另外 agent 不在任何 workshop 里时，也能按描述想起它，人也能用名字点名。
  代价：多一层安装，Claude Code、Codex 等各有各的目录，以后还要挂进容器。
  B：普通文档，例如 `~/src/pylon/docs/workshop-guide.md`，`AGENTS.md` 写这个路径。
  不用安装，但每台机器、每个容器都要有 pylon 的检出，而且路径固定。
  两种都读的是本地检出：人在 `~/src/pylon` 切到别的分支时，所有 workshop 读到的约定
  也跟着变。

  建议 A，链接指向 `~/src/pylon` 的 `main` 检出，不经 Guix 安装：合并后 `git pull`
  就生效，不用 reconfigure。这是新目录和新模块（级别“能讲”，它就是规则本身），
  开工前要人点头。
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
- 11/20：`wiki/` 进 git。公开 workshop 的 wiki 只放能公开的，私有素材放私有 workshop。
- 11/20：文档可以引用 wiki，但只能在文档末尾的参考一节里；正文的结论要写在文档本身，
  经 PR 给人看。`src`、`env` 不引用 wiki。

## 文档

- [TODO](TODO.md)：进度。
