---
description: Codex/GPT 做几步就停下来问、Opus 贵：社区观察和各自的改进办法
category: models
sources:
  - https://github.com/openai/codex/issues/58835
  - https://github.com/openai/codex/discussions/7740
  - https://news.ycombinator.com/item?id=47879092
  - https://news.ycombinator.com/item?id=46902638
  - https://www.datacamp.com/blog/claude-opus-4-8-vs-gpt-5-5
  - https://developers.openai.com/cookbook/examples/gpt-5/gpt-5_prompting_guide
  - https://developers.openai.com/cookbook/examples/gpt-5/gpt-5-1_prompting_guide
  - https://www.builder.io/blog/the-claude-advisor-pattern
  - https://dev.to/max_quimby/opus-plans-haiku-executes-the-agent-cost-stack-13h0
  - https://www.anthropic.com/claude-opus-5-5
checked: 2026-11-13
---

# 模型的能动性、实干性与成本

问题：GPT 系列（Codex）做几步就停下来问、反复确认；Opus 好很多但贵。下面是网上的评测
和心得，跑分多来自厂商公告和聚合站，只看趋势，不看具体数字，也还没有人验证过。

## 社区观察：不是错觉

- Codex “做前几步就问要不要继续”是长期被抱怨的问题，HN 每代 GPT/Codex 发布帖里都有；
  还有 issue 报告 “Approve for me” 下仍反复要求 “you can continue”（codex#58835、
  codex#7740）。
- 常见的定性评价：GPT 偏终端/脚本类短任务、快；Opus 偏仓库级、多文件、长链路任务。
  另一个说法是 Opus 不确定时倾向停下或拒绝，GPT 倾向编造，无人值守时前者更安全
  （DataCamp 对比）。

## 改进 GPT 的办法

1. 把“不许停”写进指令。OpenAI 的 prompting guide 建议加 persistence 段落（GPT-5 指南的
   agentic eagerness 一节，也讲了反向收敛）：
   ```text
   You are an agent - keep going until the user's query is completely resolved
   before ending your turn. Never stop or hand back when you encounter
   uncertainty — research or deduce the most reasonable approach and continue.
   Only stop if genuinely blocked, if a decision changes product behavior,
   or the task is complete. Keep calling tools until verification passes.
   ```
2. 关掉确认：`codex --sandbox workspace-write --ask-for-approval never`，配合上面的指令。
   有人反映模型本身仍会停，配置只能缓解。
3. 给完成条件：任务里写清什么算做完、怎么验证（测试通过、命令输出）；有可检查的终点时
   更少中途交还。
4. 外部循环：停了就自动回一句 “continue”（脚本、hook、loop），粗暴但有效。
5. 提高 reasoning effort；低 effort 下更容易浅尝辄止。

## 降低 Opus 成本的办法

1. 分层：Opus 规划、审阅，Sonnet 或 Haiku 执行。
   - Anthropic 的 advisor tool（2026-04 起 beta）：执行模型按需咨询 Opus。官方数据
     Sonnet + Opus advisor 比 Sonnet 单跑分高、比 Opus 单跑便宜约 12%（Builder.io 介绍）。
   - Claude Code 的 subagent 可单独指定模型：搜索、读日志、跑测试用 Haiku/Sonnet。
2. 调 effort：Opus 5.5 有 low/medium/high/xhigh/max，大部分活用默认或 low 就够。
3. 订阅还是 API：Max 订阅包月，重度使用比按 token 计费划算得多。
4. 版本本身在变便宜：Opus 5.5 定价 $4/$20（每百万 token 输入/输出），官方称默认设置比
   Opus 5 便宜约 40%，且步数更少。按 token 单价已不比 GPT-5.5（$5/$30）贵；“贵”更可能是
   订阅额度或用量问题，值得算一下。
5. 缓存：长 system prompt、AGENTS.md 命中 prompt cache 后读取很便宜，避免频繁改动前缀。
