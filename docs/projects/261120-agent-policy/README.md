# agent-policy

## 目标

一个私有的策略仓库管所有 agent 的配置：全局的常驻指令（人的偏好）、全局 skill，
以及 contained-agent 的权限策略。明文，agent 能读；
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
- 策略仓库不属于 pylon，也不是 workshop：它描述所有 workshop，pylon 只是其中一个。
  pylon 提供读它、执行它的机制（skills 模块，以后的 contained-agent-v2），文件格式
  由这些代码定义，所以设计在这里讨论。

公开仓库里只写这个私有仓库的用途和在宿主上的路径，不写它的内容。

## 待定

- **策略按什么分、什么格式**：跟 contained-agent-v2 一起定。v2 的设计里盒子和授权按
  area 给（uraj 里 v2 项目 11/16 的决定），所以多半是每个 area 一份。
- **批准校验的细节**（跟 v2 一起定）：GitHub App 装在哪些仓库、给什么权限；宿主上
  谁拿私钥换令牌；同步时怎么校验 `main` 上的新提交都来自人合并的 PR。agent-policy
  是免费账号下的私有仓库，没有分支保护和 ruleset（GitHub 只给 Pro 及以上的私有仓库），
  所以校验只能放在同步这一步；要分支保护就得升级 Pro。

## 决定

- 11/20：偏好和 contained-agent 策略合进同一个私有仓库，分两步：先只放偏好，
  同步用和 skills 一样只跟 `main` 的检出；再加权限策略、agent 自己的 GitHub 身份和批准校验。
- 11/20：仓库叫 `agent-policy`（`wenxin-wang/agent-policy`，私有，人已建好）。
  全局只有一个：里面没有任何一方的隐私，只有不想公开的域名、机器之类。
- 11/20：pylon 的 skills 模块负责同步这个仓库，并把其中的偏好文件链接到
  `~/.claude/CLAUDE.md` 和 `~/.codex/AGENTS.md`。skills 的影响范围因此变大，
  见 [docs/skills.md](../../skills.md)。
- 11/21：全局 skill 也放这里统一管理，包括 workshop skill，不公开。
- 11/20：在 pylon 里做；uraj 进入维护状态，不再加新功能。
- 11/26：不给真部署命令加确认。起因是 11/25 一个 session 在人给的条件满足前就部署了线上服务。
  看过三种做法：每条命令弹确认（`permissions.ask`），太耽误事；每台主机每个 session
  确认一次，要自己写 hook，而且拦不住同一 session 里先批过的主机；让另一个模型读会话
  记录判断批准了没有，和 Claude Code 的 auto mode、Codex 的 auto-review 是同一类，人
  觉得不准。Claude Code 现在用 bypassPermissions。
- 11/26：权限策略由 contained-agent-v2 读，第 2 步跟 v2 一起做，不给 uraj 的 v1 加来源。
- 11/26：agent 的 GitHub 身份用 GitHub App，不用机器人账号：私钥留在宿主上，agent 只拿
  1 小时过期的安装令牌；也在 v2 里做。在那之前 agent 用人的 token，能直接推 `main`。

## 文档

- [TODO](TODO.md)：进度。
