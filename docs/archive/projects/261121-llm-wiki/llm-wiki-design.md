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
  <主题>.md             页面：一个主题一页，agent 整理的结论；永远平铺，不分子目录
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

永远平铺，不按目录分类。类别很难定，定了以后还会变；按目录分，改类别就要挪文件，
指向它的链接都会断。分组靠 frontmatter 里的 `category`，只影响索引怎么显示，改一行就行。

```markdown
---
description: 一句话，说清这页回答什么问题
category: agents
sources:
  - https://...
  - raw/20261122-xxx.md
  - zotero://select/items/ABCD1234 （书名，作者，页码）
checked: 2026-11-22
---
```

- `description` 一句话，100 字以内，索引只显示它和文件名。
- `sources` 接受任何 URI：网址、`raw/` 里的文件、Zotero、本地书籍。本地来源换台机器或在
  容器里打不开，所以旁边写上人能看懂的出处（书名、作者、页码）；公开 workshop 里不写会
  暴露隐私的本地路径。本地来源不占仓库空间，能用就不往 `raw/` 里放。
- 一个主题一页。已有相关页面就更新它，不另开新页；页面之间用链接，不重复写同样的内容。
- 只写外部知识，也就是关于仓库以外的事。本仓库自己的事，要么能从代码读出来，要么该
  写进 `docs/` 给人看。
- `checked` 是最后核对来源的日期。

### 索引

- workshop skill 带一个脚本，读所有页面的 frontmatter，按类别分组，每页只输出文件名和
  描述；`sources`、`checked` 不输出，要用时打开页面看。Claude Code 加载 skill、Letta 加载
  记忆也是这样，只给名字和描述。
- 不提交 `index.md`：几个 agent 同时加页面时，索引文件是冲突最多的地方；描述只写在
  页面里一份，也不会和索引对不上。
- 输出控制在 9000 字节以内：Claude Code 和 Codex 实测都在 10000 字节左右截断 hook 的
  输出。英文描述大约 75 页、中文描述大约 50 页就到上限，这时只列类别和每类条数；
  `--categories` 列出全部类别，`--category <名字>` 展开一类，这两个都不折叠。
- 有问题的页面打一行 `! 文件: 原因`，单独限 1500 字节，不挤掉索引。退出码始终是 0：
  两个工具都会丢掉失败的 hook 的全部输出，退出码非零就等于静默输出空索引，agent 会以为
  llm-wiki 是空的。要检查失败用 `--check`。
- 只读当前仓库根目录下的 `llm-wiki/`；仓库外、或者没有 `llm-wiki/` 时什么都不输出。
  手动指定的路径下没有东西时，打一行说明，免得路径写错看起来像空的。

## 什么时候读、写、整理

| 时机 | 做什么 |
|---|---|
| 开工时 | 运行索引脚本。支持 SessionStart hook 的工具（Claude Code、Codex）自动运行；否则靠 skill 里的规则 |
| 调研前 | 看索引，有相关页面先读；`checked` 太旧就重新核对，更新日期 |
| 调研后 | 结论写成页面或更新已有页面，和做这件事的 PR 一起提交，PR 描述单列一节，人可以跳过 |
| 项目结束 | 整理这个项目用过的页面：合并重复、拆开过大、删掉过时；删掉没人引用的 `raw/` |

只在调研当时写，不在每一步结束时另外回顾：那样触发条件模糊，和整理决定、更新 TODO
挤在一起，agent 容易为了满足规则硬凑条目。漏写的交给定时的反思（reflection 项目）去补。
没有定期的全量检查。

## 和别处的边界

- 文档只能在末尾的参考一节引用 llm-wiki；文档依赖的结论写在文档本身，经 PR 给人看。
- `src` 和 `env` 不引用 llm-wiki。
- 各 workshop 各管各的；别的 workshop 的 agent 需要时运行对方的索引脚本。
- 进 git。公开 workshop 的 llm-wiki 只放能公开的。

## hook

hook 配在各 workshop 自己的仓库里（Claude Code 的 `.claude/settings.json`、Codex 的
`.codex/config.toml`），不改人的全局配置：人的 `~/.claude/settings.json` 是手动管理的，
程序去改它影响太大。只有一个 workshop 有 llm-wiki 时，配在仓库里也最容易验证。

- 开工时和压缩之后各运行一次索引脚本：Claude Code 和 Codex 的 SessionStart 都能匹配
  “压缩后”这个来源。压缩会丢掉开工时给的索引，这样补回来。
- 压缩前（PreCompact）让压缩摘要保留“学到了、还没写进 llm-wiki 的外部知识”。Claude Code
  的 PreCompact 只能影响摘要，不能让 agent 先去写文件；这一条借自 OpenClaw 的 memory
  flush。Codex 的 PreCompact 输出会被忽略，只能控制要不要压缩，所以 Codex 没有这一条。
- 没装 workshop skill 时（比如某个容器没装 skills 模块），hook 不报错，而是输出一行
  “找不到索引脚本，去装 workshop skill”，退出码为 0。失败的 hook 只会给人弹错误提示，
  agent 什么都看不到；输出一行说明，agent 就知道没拿到索引的原因。
- Codex 只在项目被信任时加载 `.codex/`，信任记在人自己的 Codex 配置里，不在仓库里。
  在 contained-agent 里，对配置的修改退出时就丢了，所以还没在 Codex 上验证过；等
  contained-agent-v2。

## 参考

- [agent-memory](../../../../llm-wiki/agent-memory.md)：Karpathy、Letta、
  Hermes、OpenClaw 怎么管 agent 的记忆。
- [agent-hooks](../../../../llm-wiki/agent-hooks.md)：Claude Code 和 Codex 的 hook。
