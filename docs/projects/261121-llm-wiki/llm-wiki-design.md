# llm-wiki 设计

一个 workshop 的 `llm-wiki/` 放 agent 关心、人不关心的外部知识：调研结论、方案对比、
外部文档的要点。目的是同一件事不用每个 session 重新查一遍。

## 在 agent 能看到的东西里，它处在哪

agent 的上下文分两层：

```
常驻层：每个 session 开头就在上下文里，小、经人批准
  ├─ AGENTS.md（workshop 的规则）
  ├─ 全局 skill 的名字和描述、偏好（策略仓库）
  └─ llm-wiki 的索引（脚本现场生成，几十行）        ← llm-wiki 只有这一点常驻
按需层：需要时才读，可以很大
  ├─ docs/（人读的文档）
  ├─ skill 全文
  └─ llm-wiki 的页面和素材                           ← llm-wiki 的主体
```

为什么索引要常驻：agent 不知道里面有什么，就想不到去查，最后还是重新调研。索引让它
开工时就有一张地图。为什么只有索引常驻：常驻的东西每个 session 都占上下文，越短越好；
页面按需读就够了。

各家框架（Letta、Hermes、OpenClaw）都是这个两层结构，见文末参考。

## llm-wiki 自己分三层

```
llm-wiki/
  raw/                  素材：没有稳定网址的原始材料
  <主题>.md             页面：一个主题一页，agent 整理的结论
  <类别>/<主题>.md      页面多了以后按类别分子目录
（索引不是文件，由脚本从页面的 frontmatter 生成）
```

规则写在 workshop skill 的 llm-wiki 一节，相当于 Karpathy 说的 schema 层；不在
`llm-wiki/` 里另写一份。

### 素材 `raw/`

- 只放没有稳定网址的东西：agent 自己写的调研原文、会失效的页面摘录。有稳定网址的
  素材只在页面的 frontmatter 里写链接；链接失效时先去 Wayback Machine 找。
- 公开的 workshop 里不整篇复制别人的文章（版权），只放短摘录。
- 不改，可删。项目结束整理时，删掉已经没有页面引用的；git 历史里还在。

为什么这样：Karpathy 的 raw 永久保留、整篇存放，好让页面随时能重新生成；但那是私人
wiki。我们的会越积越多，还受公开仓库的限制，所以只留找不到别处的东西。

### 页面

```markdown
---
description: 一句话，说清这页回答什么问题
category: agents
sources:
  - https://...
  - raw/20261122-xxx.md
checked: 2026-11-22
---
```

- 一个主题一页。已有相关页面就更新它，不另开新页；页面之间用链接，不重复写同样的内容。
- 只写外部知识。本仓库自己的事，要么能从代码读出来，要么该写进 `docs/` 给人看。
- `checked` 是最后核对来源的日期。

### 索引

- workshop skill 带一个脚本，读所有页面的 frontmatter，按类别分组输出“链接 + 描述”。
- 不提交 `index.md`：几个 agent 同时加页面时，索引文件是冲突最多的地方；描述只写在
  页面里一份，也不会和索引对不上。
- 输出太长时只列类别和每类条数，agent 需要时再展开一类。Codex 的 hook 默认只直接注入
  2500 token，超出的只给预览，所以索引要控制在这个以内。
- 脚本出错要报错，不能静默输出空索引，否则 agent 会以为 llm-wiki 是空的。

## 什么时候读、写、整理

| 时机 | 做什么 |
|---|---|
| 开工时 | 运行索引脚本。支持 SessionStart hook 的工具（Claude Code、Codex）自动运行；否则靠 skill 里的规则 |
| 调研前 | 看索引，有相关页面先读；`checked` 太旧就重新核对，更新日期 |
| 调研后 | 结论写成页面或更新已有页面，和做这件事的 PR 一起提交，PR 描述单列一节，人可以跳过 |
| 每一步结束 | 回顾这一步学到的外部知识，没写的补上 |
| 项目结束 | 整理这个项目用过的页面：合并重复、拆开过大、删掉过时；删掉没人引用的 `raw/` |

没有定期的全量检查。定时触发的后台反思放在 reflection 项目里。

## 和别处的边界

- 文档只能在末尾的参考一节引用 llm-wiki；文档依赖的结论写在文档本身，经 PR 给人看。
- `src` 和 `env` 不引用 llm-wiki。
- 各 workshop 各管各的；别的 workshop 的 agent 需要时运行对方的索引脚本。
- 进 git。公开 workshop 的 llm-wiki 只放能公开的。

## 还没定

见 [README](README.md#待定)：hook 用不用来做压缩后重给索引、压缩前提醒；第一批三篇
素材放进哪个 workshop。

## 参考

- [agent-memory-frameworks](../../../llm-wiki/agent-memory-frameworks.md)：Karpathy、Letta、
  Hermes、OpenClaw 怎么管 agent 的记忆。
- [agent-hooks](../../../llm-wiki/agent-hooks.md)：Claude Code 和 Codex 的 hook。
