# reflection

## 目标

agent 定期回顾一段时间里的工作，把值得留下的东西整理成 PR 交给人审：外部知识进
llm-wiki，对人的了解和协作上的教训进全局记忆或偏好，约定上的问题提给 workshop skill。
人另外能一眼看到各 workshop 里在等自己的事。做完之后，同样的纠正人不用说第二遍。

## 背景

各家 agent 框架都有一个整理记忆的环节（Letta 的 sleep-time 反思、Hermes 的后台
回顾、OpenClaw 的 dreaming），不整理，记忆就会膨胀、过时。workshop 现在只有“每一步
结束时整理”，靠做事的 agent 顺手完成，只看得到自己这一步。

11/22 人和 agent 讨论过一个“全局事务性记忆”：最近关心什么、各处状态、反思结果。
其中各处状态能从各 workshop 的 `TODO.md` 现场汇总，不用另存；“最近关心什么”是人的
想法，由人写；只有反思结果是 agent 写的记忆，放在这个项目里定。

llm-wiki 项目（已结束）定了：外部知识只在调研当时写进 llm-wiki，不在每一步结束时另外
回顾，“漏写的交给定时的反思去补”。所以反思对 llm-wiki 有一项确定的活。它也留下了一套
可以照搬的做法：常驻的只是一份由脚本现场生成的短索引，靠仓库里的 SessionStart hook 在
开工和压缩后给 agent。见 [llm-wiki 设计](../../archive/projects/261121-llm-wiki/llm-wiki-design.md)。

agent 写进常驻上下文的东西，相当于给以后所有 agent 下指令，所以产出要经人批准；工作相关
的内容不能进个人的仓库。

## 待定

前三条决定第 2 步（手动跑一轮）怎么跑，先定；后三条到第 3、4 步再定也来得及。

- **读什么。**
  A：Claude Code 和 Codex 的本地会话记录。最全，人在会话里纠正 agent 的地方最有价值；
  代价：里面什么都有，读它的 agent 能看到所有 workshop 的内容。
  B：合并的 PR 和 `TODO.md` 的变化。干净，但看不到过程和纠正。
  建议 A 为主、B 补充，但按暴露范围分开读：个人机器只读个人的会话。Claude Code 的记录在
  宿主的 `~/.claude/projects/` 下，按工作目录分；容器里跑的 Codex 的记录保不保得住，
  第 2 步先查清楚（容器退出会丢掉配置修改，记录可能也一样）。
- **产出写到哪、怎么提交。** 都走 PR，攒一批一起提。
  - 外部知识：进对应 workshop 的 `llm-wiki/`，格式照 workshop skill。两件事：补上会话里
    查过、没写进去的；顺带整理：`checked` 太旧的重新核对，索引里的 `!` 行修掉，重复的合并。
  - 协作上的教训：提炼成偏好或 skill 的改动，向策略仓库提 PR。
  - 对人的了解：原想进策略仓库里一个新的全局记忆文件，常驻加载，设硬上限（参考 Hermes，
    几千字符，写满报错逼着取舍）；它和偏好的分界：偏好是人的要求，记忆是 agent 观察到的。
  建议第 2 步只做前两类，全局记忆等第一轮跑出来、看到实际产出再定。
- **agent 的工作记忆放在哪、活多久、谁能读。** 一个 session 里讨论常常跨好几个仓库，
  冒出来的事要有地方先记下，agent 觉得“快做完了”时回来看一遍，免得漏掉没问的问题。
  按时间尺度，它在最短的一端：session 待办（一个 session）→ `TODO.md`（一个项目）→
  llm-wiki（外部知识）→ 反思的记忆（对人的了解，常驻）。
  现在放在 worktree 的 `tmp/`：容器销毁还在，worktree 删掉就没了，也只有这个 session 看得到；
  压缩之后要靠 agent 自己记得去读。
  A：每个 session 一个目录，放在 workshop 之外、宿主上的固定位置（比如
  `~/.local/state/agents/sessions/<id>/`），容器挂进去；反思可以直接读它。
  B：放进 workshop 的 git。持久、可见，但每个 session 的零碎待办会变成提交和冲突。
  建议 A，再照 llm-wiki 的做法加 hook：压缩后把待办打出来，压缩前提醒摘要保留还没记下的事。
  hook 拿得到 session id 才能找到目录，Claude Code 的 hook 输入里有，Codex 的还没核实。
  以后它也可能当 agent 之间的信道，但那很像之前作废的信箱设计，到时要防止又做复杂。
  定下来后写进 workshop skill。
- **在哪跑、多久一次。**
  A：宿主上的定时器（Guix Home 的 shepherd timer）。简单；代价：在容器外跑 agent，
  权限要另外收紧。B：contained-agent 的容器里，由定时器启动。权限和挂载复用现有的；
  代价：要等 contained-agent-v2，而 uraj-migration 挂起着。
  建议现在不定：第 2 步在普通 session 里手动跑，第 3 步再看那时 v2 的进度。部署由人来做。
- **人看什么。**
  一个脚本汇总所有 workshop 里的项目状态，重点列出“等人”的事项；人自己写一个短的
  “最近关心什么”文件，放在策略仓库，反思时拿它作参照。
  概览放哪：A，像 llm-wiki 索引那样由 SessionStart hook 给每个 session，agent 也能看到
  别处在等什么；代价是常驻的又多一块，hook 输出在 10000 字节左右会被截断。B，只在人要的
  时候运行。建议先 B，用一阵再看要不要常驻。
- **工作机。** 工作机单独一个策略仓库（比如 `agent-policy-work`），装在同一个
  `~/.local/share/agent-policies/` 下；工作相关的记忆、skill、反思产出都进那里。
  不影响个人机器上的第 2 步，用到工作机时再定。

## 决定

- 11/22：开这个项目。反思由定时器触发，不绑在 session 上；范围是“agent 回顾、人审阅”
  这个循环，属于人机合作，不是 agent 之间的合作。

## 文档

- [TODO](TODO.md)：进度。
