# reflection 设计

第 1 步还没走完，这里先写已经定下的部分：读什么、线索、session 笔记、产出。

## 读什么

主要读 Claude Code 和 Codex 的本地会话记录，PR 和 `TODO.md` 的变化作补充。只看 PR
看不到过程，而人在会话里纠正 agent 的地方最有价值。

会话记录很大，不能整篇交给模型。11/23 量过：过去 7 天 `~/.claude/projects/` 下有 308 个
session，共 362MB，中位数每个 675KB。拆开最大的一个：工具输出（文件内容、命令结果）约
200 万字符，工具调用约 22 万，人说的话加系统注入的文本约 6 万，agent 的回复不到 1 万。
有价值的部分只占 3% 左右。所以：

1. 先用脚本过滤：去掉工具输出，只留人说的话和 agent 的回复。这一步不用模型。
2. 先读线索（见下一节），从线索指到的地方开始看。
3. 过滤后的记录再全扫一遍兜底，线索漏掉的东西也丢不了。线索只决定先看哪里。

读记录、找教训这类量大的活用便宜的模型（比如 Sonnet），只有写提议时才考虑换更强的。

按暴露范围分开读：个人机器只读个人的会话，读它的 agent 能看到的也就是这些。容器里跑的
Codex 的记录退出后还在不在，第 2 步先查清楚（容器退出会丢掉配置修改，记录可能也一样）。

## 线索

线索是 session 进行时就留下的标记：“这里人纠正了 agent”“这件事值得写进 llm-wiki”。

不靠 agent 自觉记。Claude Code 的自动记忆就是靠 agent 自己判断，pylon 下几天里只记了
4 条，其中两条是人明确表达不满之后才记的。第三方插件 claude-reflect 也是用 hook 机械地
抓，不交给 agent。所以线索有三个来源，前两个不需要 agent 配合：

- 人发消息时的 hook（Claude Code 的 `UserPromptSubmit`）：认出纠正，记进 session 笔记。
  用正则会漏掉换了说法的，用模型就用便宜的；第 2 步再定用哪种。
- 压缩前的 hook：让摘要保留还没记下的事，和 llm-wiki 的压缩前提醒同一个做法。Codex 的
  PreCompact 影响不了摘要，没有这一条。
- Claude Code 已有的自动记忆（`~/.claude/projects/<repo>/memory/`）：反思时直接读。

第 2 步顺便量效果：对比“只看线索找到的”和“全扫找到的”。线索找不到多少东西，就不继续做。

## session 笔记

每个 session 一份 session 笔记，两块：待办，给这个 session 自己用，觉得“快做完了”时
回来看一遍；线索，给以后的反思用。一份文件、同一套 hook：压缩后把笔记打出来，压缩前提醒
摘要保留还没记下的事。

为什么合成一份：两者都是 session 里随手记、session 结束就不再改的东西，hook 要做的事也
一样。待办现在放在 worktree 的 `tmp/`，压缩后要靠 agent 自己记得去读，worktree 删掉就没了，
反思也读不到。

hook 要拿到 session id 才能找到笔记。Claude Code 的 hook 输入里有，Codex 的还没核实。

## 产出

每条提议直接提 PR 到它所属的地方，在那个 PR 里和人商量；攒一批一起提。

- 外部知识：进对应 workshop 的 `llm-wiki/`，格式照 workshop skill。补上会话里查过、没写
  进去的；顺带把 `checked` 太旧的重新核对，索引里的 `!` 行修掉，重复的合并。
- 做事方式：看它该放哪：只关于一个 workshop 的，进那个 workshop（`AGENTS.md` 或项目
  skill）；所有 workshop 都适用的，向策略仓库提偏好或全局 skill 的改动。

每条都写明证据：哪个 session、人当时怎么说的。提炼出来的教训也可能是错的，出现次数多
不等于对；有出处，人审的时候才查得了，教训以后不适用了也知道当初为什么有它。

## 参考

- [agent-memory](../../../llm-wiki/agent-memory.md)：各家怎么记，claude-reflect、Claude Code
  自动记忆、Codex memories 的做法，以及记忆出错的教训。
