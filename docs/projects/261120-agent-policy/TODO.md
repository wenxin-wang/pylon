# agent-policy 进度

状态：进行中。

当前：第 2 步。

1. ~~偏好~~ 已完成：私有仓库里放 `preferences.md`（agent-policy PR #1）；skills 模块同步它并链接到各工具的全局说明文件（pylon PR #5）。
2. 每个 workshop 的策略：先定按什么分和格式，再做机器人账号、ruleset、带批准校验的同步，策略内容迁进来。
   - 定权限时一起讨论：真部署命令（不带 `--check` 的部署脚本和 `ansible-playbook`、
     `guix deploy`、`herd restart`、`systemctl restart` 等）弹确认，bypassPermissions 下也弹；
     用 `ask` 规则还是 PreToolUse hook。起因：11-25 一个 session 在人给的条件满足前就部署了
     线上服务；规则写在全局偏好和记忆里都没拦住，部署那一刻也没有东西停下来。文档说 `ask`
     规则在 bypassPermissions 下照样弹；hook 返回 ask 是否也弹、拼起来的命令（`cd …; deploy.sh`）
     能不能匹配上，还没测。
