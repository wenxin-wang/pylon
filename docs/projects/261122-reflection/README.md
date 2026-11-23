# reflection

## 目标

agent 定期回顾一段时间里的工作，把值得留下的东西整理成 PR 交给人审：外部知识进
llm-wiki，做事方式提到它所属的地方（某个 workshop，或策略仓库里的偏好和全局 skill）。
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

- **session 笔记放在哪。** 一个 session 里讨论常常跨好几个仓库，笔记不属于哪个 workshop。
  A：每个 session 一个目录，放在 workshop 之外、宿主上的固定位置（比如
  `~/.local/state/agents/sessions/<id>/`），容器挂进去；反思可以直接读它。
  B：放进 workshop 的 git。持久、可见，但每个 session 的零碎记录会变成提交和冲突。
  建议 A。以后它也可能当 agent 之间的信道，但那很像之前作废的信箱设计，到时要防止又做
  复杂。定下来后写进 workshop skill。
- **全局记忆。** 对人的了解（agent 观察到的，区别于人提的要求，那是偏好）要不要一个常驻的
  文件，放在策略仓库，设硬上限（参考 Hermes，几千字符，写满报错逼着取舍）。建议等第一轮
  反思跑出来、看到实际产出再定。
- **在哪跑、多久一次。** A：宿主上的定时器（Guix Home 的 shepherd timer）。简单；代价：
  在容器外跑 agent，权限要另外收紧。B：contained-agent 的容器里，由定时器启动。权限和挂载
  复用现有的；代价：要等 contained-agent-v2，而 uraj-migration 挂起着。第 4 步再定；部署由
  人来做。PR 里的讨论在那之前也是人开 session 让 agent 去回复。
- **状态概览放哪。** A：像 llm-wiki 索引那样由 SessionStart hook 给每个 session，agent 也能
  看到别处在等什么；代价是常驻的又多一块。B：只在人要的时候运行。建议先 B。另外，人写一个
  短的“最近关心什么”文件放在策略仓库，反思时拿它作参照，要不要也到第 5 步再定。

## 决定

- 11/22：开这个项目。反思由定时器触发，不绑在 session 上；范围是“agent 回顾、人审阅”
  这个循环，属于人机合作，不是 agent 之间的合作。
- 11/23：先读线索，再全扫过滤后的会话记录兜底；读记录用便宜的模型（比如 Sonnet）。见
  [设计](reflection-design.md#读什么)。
- 11/23：线索靠 hook 和 Claude Code 的自动记忆，不靠 agent 自觉记；第 2 步量效果。见
  [设计](reflection-design.md#线索)。
- 11/23：待办和线索合成每个 session 一份 session 笔记，同一套 hook。见
  [设计](reflection-design.md#session-笔记)。
- 11/23：每条提议直接提 PR 到它所属的 workshop 或策略仓库，在那个 PR 里商量。见
  [设计](reflection-design.md#产出)。
- 11/23：做状态概览：一个脚本汇总各 workshop 的项目状态，“等人”的排在最前。

## 文档

- [TODO](TODO.md)：进度。
- [设计](reflection-design.md)：读什么、线索、session 笔记、产出，以及为什么。
