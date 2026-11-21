# workshop-design 进度

状态：等人：在 contained-agent 里加只读挂载 `~/.local/share/agent-policies`，验证容器里能读到 workshop skill，再合 PR #6。

当前：第 2 步。

1. ~~skill 怎么装~~ 已完成。skills 模块，全局 skill 和偏好来自策略仓库；PR #5。
2. 拆约定：把 pylon 的 `AGENTS.md` 分成通用部分（策略仓库的 workshop skill，
   agent-policy PR #2）和 pylon 特有部分（PR #6）。等容器能读到 skill 后合并。
3. ~~wiki、记忆、「决定」的写法~~ 已写进 workshop skill（agent-policy PR #2）。
4. ~~人的偏好放在哪~~ 已完成：策略仓库的 `preferences.md`（agent-policy PR #1）。
