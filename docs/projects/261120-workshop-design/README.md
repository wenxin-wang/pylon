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

- **人的偏好放在哪。** 各工具自带的 memory 是私有的，换个工具就没了；人的偏好
  （例如时间一律用北京时间）希望所有工具都知道。选项：
  A：写进 workshop skill；只在 workshop 里生效，而且 skill 是按需读的。
  B：一份偏好文件，由 skills 模块链接到各工具的全局说明文件（Claude Code 的
  `~/.claude/CLAUDE.md`、Codex 的 `~/.codex/AGENTS.md`）。这两个文件每个 session
  都会加载，不在 workshop 里也生效。代价：skills 模块多管两个文件，影响范围变大；
  pylon 是公开的，偏好里不能有私人内容。
  C：每个工具各自记，人重复说。
  建议 B。

## 决定

- 11/20：先做 workshop 本身的设计，uraj-migration 挂起。
- 11/20：不做派活的工具和信箱。agent 以某个 workshop 为默认工作目录，可以改自己的
  workshop、关联的仓库，以及别的 workshop，暂不限制范围。agent 之间怎么合作以后摸索，
  很快会单独讨论。
- 11/20：每个 workshop 可以有 `wiki/`，放 agent 关心、人不关心的知识和素材；
  不再有独立的 llm-wiki。
- 11/20：约定做成 skill。每个 workshop 的 `AGENTS.md` 写必须时刻成立的几条、
  这个 workshop 自己的东西，再要求开工前先读这个 skill。skill 不跟着 pylon 切分支。
- 11/20：skill 的源文件放在 pylon 顶层的 `skills/<name>/`。安装：单独一份只跟 `main`
  的 pylon 检出，放在 `~/.local/share/pylon/main`，用 `git` 更新，各工具的 skill 目录
  用符号链接指过去；不经 Guix，不用 reconfigure。备选是单独一个 Guix profile（快、
  能回退、容器共享 store 时直接可见），容器需要时再看。
- 11/20：新增两个模块：workshop（workshop 约定这个 skill，能讲）、skills（skill 的
  来源、版本和安装，能管）。
- 11/20：读 space 暂不另定规则，沿用 `AGENTS.md` 里列出的。现在 space 里内容很少、
  也不算敏感，以后由 contained-agent-v2 的挂载限制。
- 11/20：agent 的记忆：进度只写 `TODO.md`，agent 关心的知识进 `wiki/`，
  工具自带的 memory 不放别的 agent 也需要的东西。
- 11/20：`wiki/` 进 git。公开 workshop 的 wiki 只放能公开的，私有素材放私有 workshop。
- 11/20：文档可以引用 wiki，但只能在文档末尾的参考一节里；正文的结论要写在文档本身，
  经 PR 给人看。`src`、`env` 不引用 wiki。

## 文档

- [TODO](TODO.md)：进度。
