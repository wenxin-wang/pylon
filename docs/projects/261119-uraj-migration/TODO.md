# uraj-migration 进度

状态：挂起：uraj 已进入维护，迁移不急；恢复时先定各模块的级别和第一个迁哪个。

当前：第 1 步。

1. 定级别和顺序：给 uraj 的每个模块定级别，排出迁移顺序。等人定。
2. 迁第一个模块：按定下的顺序。

迁移时顺带：

- 迁 contained-agent-v2（含 sudo-tiers 的审批通道）时，更新 `llm-wiki/sudo-approval.md`，
  它停在 11/02 的早期调研。
- 迁 contained-agent-v2 时，让 Codex 的项目信任（或整个 `~/.codex`）在容器之间保留下来，
  再新开 Codex session 验证 pylon 的 `.codex/config.toml`：开工和压缩后都拿到 llm-wiki
  索引。现在的 contained-agent 退出时丢掉配置修改，项目没被信任，hook 不加载。
