# reflection 进度

状态：进行中。

当前：第 2 步。

1. ~~定设计~~ 已完成：读什么、线索、session 笔记、产出、在哪跑，见[设计](reflection-design.md)。
   全局记忆留到第 3 步。不做汇总项目状态的脚本，改在 workshop skill 里加一句
   （agent-policy PR #11）。
2. session 笔记：目录、hook（开工时清掉 30 天前的笔记，压缩前后，人发消息时抓纠正），
   写进 workshop skill。
3. 手动跑一轮：过滤会话记录的脚本；agent 按设计回顾一段时间，提出第一批 PR，看产出值不值；
   对比只看线索和全扫各找到什么；查清容器里 Codex 的记录保不保得住；定全局记忆。
4. 自动化：等 contained-agent-v2；定时器启动容器里的反思，定多久一次，部署由人来做。
5. 工作机：单独一个策略仓库（比如 `agent-policy-work`），装在同一个
   `~/.local/share/agent-policies/` 下；工作相关的记忆、skill、反思产出都进那里。
