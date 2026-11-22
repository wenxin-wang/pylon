---
description: 常驻个人 agent 的候选（OpenClaw、Hermes、dsh、Muse、Grok Bot、自建）和要回答的问题
category: agents
sources:
  - https://docs.openclaw.ai/
  - https://hermes-agent.nousresearch.com/
  - https://github.com/deepseek-ai/deepseek-harness
  - https://ai.meta.com/muse/
  - https://x.ai/bot
checked: 2026-11-13
---

# 常驻 agent：harness 选型与自建边界

“常驻个人 agent（always-on personal agent）”：能持续接收任务、保留工作状态、关掉交互
session 后继续推进并回报。这一页只有候选和问题，还没有比较结论；当时的详细调研报告
没有保存下来。

## 起点

- uraj 里已经部署了 OpenClaw。
- 有说法称很多人已经从 OpenClaw 转到 DeepSeek Harness 的一键配置，Muse、Grok Bot 等新
  产品也值得看。没有出处，使用趋势和“一键配置”的范围都有待核实。
- 选型同时看成品服务、常驻运行设施和执行 harness，分别比较它们解决的部分。

## 候选

| 候选 | 要考察什么 |
|---|---|
| OpenClaw | 现有部署的能力、痛点，继续用或局部替换的成本 |
| Hermes Agent | 社区迁移心得里的维护和记忆体验，现有配置的迁移成本 |
| DeepSeek Harness（dsh） | 一键启动能覆盖多少常驻需求，插件扩展适不适合做自己的底座 |
| Meta Muse | 个人 agent 的使用体验，和本地工作流的适配程度 |
| Grok Bot | 长期任务、协作和工具集成，状态和权限能否符合 workshop 的约束 |
| 基于现有组件自建 | 自己掌握哪些层，哪些能力直接用现成实现 |

Hermes 和 OpenClaw 的记忆设计见 [agent-memory](agent-memory.md)。

## 要回答的问题

- “常驻”具体要哪些保证：随时能找、主动唤醒、关掉客户端仍工作、服务重启后恢复？
- 候选在同一任务上的完成质量、人工接管次数、耗时和总成本如何？
- 哪些能直接操作本地 Org、Git 和工具，哪些要额外的同步或桥接？
- 长期知识、任务状态和授权边界怎样保留，不依赖某一个 session 或厂商？
- 自建是为了改善自己的工作流，还是也要追上成品的连接器、手机入口和浏览器体验？
