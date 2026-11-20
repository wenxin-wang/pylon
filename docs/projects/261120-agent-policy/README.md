# agent-policy

## 目标

一个私有仓库管 agent 的配置：常驻指令（人的偏好）和权限策略。明文，agent 能读；
改动由 agent 提 PR，人批准合并后才生效，不用 reconfigure。合并后，人不用再给每个
工具、每台机器重复说偏好，agent 也能看到自己有什么权限。

## 背景

- 人的偏好（例如时间一律用北京时间）现在散在各工具自己的 memory 里，换个工具就没了。
  Claude Code 的 `~/.claude/CLAUDE.md`、Codex 的 `~/.codex/AGENTS.md` 是每个 session
  都会加载的全局说明文件，适合放偏好。
- 常驻指令会进每个 session，等于给以后所有 agent 下指令。agent 如果能自己改，就和
  自己给自己加权限是一类风险，所以和权限策略走同一道审批。
- contained-agent 的策略现在有几处来源：公开的要 reconfigure，私有的只有人能改、
  agent 读不到，加密的要在主机上 reconfigure。大多数内容不是秘密，只是不想公开。
  结果 agent 看不到自己的权限，排查问题全靠人转述。
- uraj 进入维护状态，不再加新功能，所以这件事在 pylon 里做。

公开仓库里只写这个私有仓库的用途和在宿主上的路径，不写它的内容。

## 待定

- **策略那一步的细节**（第 2 步再定）：机器人账号、`main` 的 ruleset、同步时校验
  最新提交来自被批准的 PR、策略由谁读（uraj 的 contained-agent 还是 pylon 里的 v2）。

## 决定

- 11/20：偏好和 contained-agent 策略合进同一个私有仓库，分两步：先只放偏好，
  同步用和 skills 一样只跟 `main` 的检出；再加权限策略、机器人账号和批准校验。
- 11/20：仓库叫 `agent-policy`（`wenxin-wang/agent-policy`，私有，人已建好）。
- 11/20：pylon 的 skills 模块负责同步这个仓库，并把其中的偏好文件链接到
  `~/.claude/CLAUDE.md` 和 `~/.codex/AGENTS.md`。skills 的影响范围因此变大，
  见 [docs/skills.md](../../skills.md)。
- 11/20：在 pylon 里做；uraj 进入维护状态，不再加新功能。

## 文档

- [TODO](TODO.md)：进度。
