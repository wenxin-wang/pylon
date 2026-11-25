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
- 迁 contained-agent-v2 时，要能由定时器启动容器里的 session，跑反思（见
  [reflection](../261122-reflection/README.md) 第 4 步）。
- 迁 contained-agent-v2 时，可读写地挂载宿主的 `~/.local/state/agents/`，session 笔记
  才能在容器退出后留下来（见 [reflection](../261122-reflection/README.md) 第 3 步）。
- 迁 contained-agent-v2 时，调研怎样让 agent 的环境只拿到它该用的秘密。把某一个秘密单独
  移出环境，一次能做到；但秘密不都天然分出级别，要找通用的办法。
- 迁 contained-agent-v2 时（或做完之后），让 agent 写的脚本默认用一个有额度的 API key：
  Claude Code 自己的 key 改由 `apiKeyHelper` 读，不放进环境变量，Bash 里只看得到有额度的
  那个；额度用完由人去加。先验证两个 key 同时在时 Claude Code 用哪个，再定变量名；Codex
  若走同一个网关，一起处理。agent-policy 同时加一句：额度用完就报告花费并停下，不找别的
  key 绕过去。起因：11/24 的重放脚本从环境变量拿到了 Claude Code 自己那个不限额的 key。
