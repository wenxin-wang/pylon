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

- **skill 怎么装。** 要求：不跟着 `~/src/pylon` 切分支；更新不用 `guix home reconfigure`
  （太慢）。这会是一个新模块（暂名 skills），管 skill 的来源、版本和安装；
  workshop 约定是其中第一个 skill。选项：
  A：单独一份 pylon 的检出，只跟 `main`，例如 `~/.local/share/pylon/main`，
  用 `git pull --ff-only` 更新；各工具的 skill 目录用符号链接指过去。最快最简单，
  不经过 store；回退靠 git。容器要另外只读挂载这个目录。
  B：单独一个 Guix profile，只装 skill 这一个包，例如
  `guix package -p <profile> -f <skills.scm>`，从 pylon 的某个提交构建。只构建这一个包，
  比 reconfigure 快得多；有代数，可以 `--roll-back`；内容在 store 里只读，
  容器共享 store 时直接可见。代价是每次更新都要过一遍 Guix。
  C：Guix Home 的 activation：activation 脚本只在 reconfigure 时跑，内容放在 store 里
  就还是要 reconfigure，解决不了慢。它能做的是一次性装好“链接”和更新命令，之后内容
  按 A 或 B 更新。
  建议先 A，加一条更新命令；容器需要时再看要不要换 B。符号链接这一次性的设置
  以后放进 Guix Home（C 的用法）。`reconfigure` 慢本身是另一个问题，迁 guix 模块时再看。
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
- 11/20：约定做成 skill。每个 workshop 的 `AGENTS.md` 写必须时刻成立的几条、
  这个 workshop 自己的东西，再要求开工前先读这个 skill。skill 不跟着 pylon 切分支。
- 11/20：`wiki/` 进 git。公开 workshop 的 wiki 只放能公开的，私有素材放私有 workshop。
- 11/20：文档可以引用 wiki，但只能在文档末尾的参考一节里；正文的结论要写在文档本身，
  经 PR 给人看。`src`、`env` 不引用 wiki。

## 文档

- [TODO](TODO.md)：进度。
