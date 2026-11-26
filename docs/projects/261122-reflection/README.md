# reflection

## 目标

agent 定期回顾一段时间里的工作，把值得留下的东西整理成 PR 交给人审：外部知识进
llm-wiki，做事方式提到它所属的地方（某个 workshop，或策略仓库里的偏好和全局 skill）。
做完之后，同样的纠正人不用说第二遍。

## 背景

各家 agent 框架都有一个整理记忆的环节（Letta 的 sleep-time 反思、Hermes 的后台
回顾、OpenClaw 的 dreaming），不整理，记忆就会膨胀、过时。workshop 现在只有“每一步
结束时整理”，靠做事的 agent 顺手完成，只看得到自己这一步。

11/22 人和 agent 讨论过一个“全局事务性记忆”：最近关心什么、各处状态、反思结果。
各处状态写在各 workshop 的 `TODO.md` 里，要看时现场读，不用另存；“最近关心什么”是人的
想法，由人写；只有反思结果是 agent 写的记忆，放在这个项目里定。

llm-wiki 项目（已结束）和反思有两处关系，见
[llm-wiki 设计](../../archive/projects/261121-llm-wiki/llm-wiki-design.md)：

- 调研时查到的外部知识，当时就写进 llm-wiki；当时漏写的，llm-wiki 项目定了交给反思去补。
  所以补 llm-wiki 是反思一定要做的事。
- llm-wiki 的索引是这样送到 agent 面前的：仓库里配一个 SessionStart hook，开工和压缩后
  各运行一次脚本，把输出放进 agent 的上下文。session 笔记照这样做。

agent 写进常驻上下文的东西，相当于给以后所有 agent 下指令，所以产出要经人批准；工作相关
的内容不能进个人的仓库。

## 待定

暂无。

## 决定

- 11/22：开这个项目。反思由定时器触发，不绑在 session 上；范围是“agent 回顾、人审阅”
  这个循环，属于人机合作，不是 agent 之间的合作。
- 11/23：每次全扫过滤后的会话记录，不做线索（第一轮检验下来不值）；读记录用便宜的模型
  （比如 Sonnet）。见[设计](reflection-design.md#为什么不做线索)。
- 11/23：读哪些会话，由各 workshop 自己认领；怎么认领，第 4 步定。见
  [设计](reflection-design.md#读什么)。
- 11/24：读过的不再读：每个 workshop 在宿主上记一个时间点，这之前的消息算读过。见
  [设计](reflection-design.md#读什么)。
- 11/26：每个 session 一份 session 笔记，只放待办；放在宿主上、所有 workshop 之外，不进
  git；30 天没动过的由开工的 hook 删掉。脚本和说明做成策略仓库里的全局 skill，要能给
  Claude Code 以外的工具用；hook 配置由 skills 模块写进 Claude Code 的配置。见
  [设计](reflection-design.md#session-笔记)。
- 11/23：每条提议直接提 PR 到它所属的 workshop 或策略仓库，在那个 PR 里商量；对人的观察
  不单设全局记忆，改写成偏好。见[设计](reflection-design.md#产出)。
- 11/24：补 llm-wiki 的 PR 先由另一个 agent 对上游审一轮，再交给人。见
  [设计](reflection-design.md#产出)。
- 11/24：反思提的 PR 都打 `reflection` label；每轮先看以前这些 PR 和审阅意见，已有的、被
  否掉的教训不再提。见[设计](reflection-design.md#产出)。
- 11/23：反思在 contained-agent-v2 的容器里由定时器启动，等 v2。见
  [设计](reflection-design.md#在哪跑)。
- 11/23：不做汇总各项目状态的脚本，也不默认给每个 session（对目标明确的 session 是干扰）；
  只在 workshop skill 里提一句：人问下一步做什么时，读各项目 `TODO.md` 的状态行，等人的先说。
- 11/23：“最近关心什么”不在这个项目里做：人自己记在 spaces 里；agent 要不要读，等 OpenClaw
  的改进开工时再讨论。

## 文档

- [TODO](TODO.md)：进度。
- [设计](reflection-design.md)：读什么、为什么不做线索、session 笔记、产出、在哪跑。
- [第一轮全扫](reflection-measure-20261123.md)：11/23，范围、做法、结果、正则检验。
