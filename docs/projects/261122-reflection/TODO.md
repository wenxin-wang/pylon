# reflection 进度

状态：进行中。

当前：第 3 步。

1. ~~定设计~~ 已完成：读什么、线索、session 笔记、产出、在哪跑，见[设计](reflection-design.md)。
   不做汇总项目状态的脚本，改在 workshop skill 里加一句
   （agent-policy PR #11）。
2. ~~手动全扫一轮~~ 已完成：过滤会话记录的脚本（`exp/reflection_20261123/`）；agent 读
   个人仓库的全部记录，提出第一批 PR，看产出值不值；拿全扫找到的纠正检验抓纠正的正则；
   定全局记忆。
   容器里 Codex 的记录退出后还在，已查清，见[设计](reflection-design.md#读什么)。
   - 11/23 跑完第一轮（[记录](reflection-measure-20261123.md)）：提了策略仓库 #12（偏好）、
     #13（workshop skill 一句）。正则召回 13%，不做线索；全局记忆不单设，写成偏好。
   - 11/23 外部知识 42 条：15 条当时已写进文档（多在 uraj）或 llm-wiki；15 条补进 llm-wiki
     （paseo、contained-agent-v1、guix-pitfalls、claude-code-auto-mode、feishu-coding-agent、
     r8152-mac-reset），对上游核对后大多要修正或收窄；其余 12 条是 uraj 自己的事、一次性的、
     没法核实的，或不该公开。
3. session 笔记，只放待办：策略仓库里的全局 skill `session-notes`（说明和 hook 脚本），
   hook 配置由人贴进 `~/.claude/settings.json`。
4. 自动化：等 contained-agent-v2；定时器启动容器里的反思，定多久一次，定各 workshop 怎么
   认领自己的会话，定“读到哪”的时间点放哪，部署由人来做。
   会话记录只留 30 天：v2 在 12/24 前做不完的话，先手动跑第二轮，从第一轮读到的 11/24 00:32
   （北京时间）读起。
5. 工作机：单独一个策略仓库（比如 `agent-policy-work`），装在同一个
   `~/.local/share/agent-policies/` 下；工作相关的记忆、skill、反思产出都进那里。
