# llm-wiki

## 目标

定下 workshop 的 `llm-wiki/` 怎么用：里面怎么组织，agent 什么时候读、什么时候写，
怎么防止内容过时。定完之后，agent 做调研前会先查它，调研完会把结论放进去，
同一件事不用每个 session 重新查一遍。

## 背景

workshop skill 只定了底线：每个 workshop 可以有 `llm-wiki/`，放 agent 关心、人不关心
的知识和素材；它进 git，所以公开 workshop 里只放能公开的；文档只能在末尾的参考一节
引用它；`src` 和 `env` 不引用它；内部结构暂时自由。

之前在人的一个 space 下试过一个 llm-wiki，没有成形的设计，只积累了一篇调研。另外还有
两篇 agent 调研没有合并。这三篇等设计定下后再搬进来，作为第一批内容。

## 待定

暂无。

## 决定

11/23 项目结束，决定都已有去处：

- 目录叫 `llm-wiki/`、分层、读写和整理的时机、边界、后台反思交给定时器：workshop skill 的
  llm-wiki 一节，理由在 [llm-wiki-design](llm-wiki-design.md)。
- hook 配在各 workshop 自己的仓库里、开工和压缩后给索引、压缩前提醒：
  [llm-wiki-design](llm-wiki-design.md) 的 hook 一节；配置在 `.claude/settings.json`、
  `.codex/config.toml`。
- AGENTS.md 的布局表加 `.claude/`、`.codex/` 一行：`AGENTS.md` 本身。
- 三篇素材放进 pylon 的 `llm-wiki/`：已搬（PR #14）。
- Codex 的 hook 推迟到 contained-agent-v2 再验证：
  [uraj-migration 的 TODO](../../../projects/261119-uraj-migration/TODO.md)。

## 文档

- [TODO](TODO.md)：进度。
- [llm-wiki-design](llm-wiki-design.md)：llm-wiki 分几层、每层放什么、什么时候读写。

## 参考

- [agent-memory](../../../../llm-wiki/agent-memory.md)：Karpathy、Letta、
  Hermes、OpenClaw 怎么管 agent 的记忆。
- [agent-hooks](../../../../llm-wiki/agent-hooks.md)：Claude Code 和 Codex 的 hook。
