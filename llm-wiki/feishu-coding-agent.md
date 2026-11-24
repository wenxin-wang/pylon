---
description: 从飞书驱动 Claude Code/Codex 的几条路径（OpenClaw、cc-connect 等）和核对过的限制
category: agents
sources:
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/openclaw/scope-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/openclaw/sandbox-guide.md
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/providers/openai/runtimes.md
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/extensions/openai/base-url.ts
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/extensions/openai/provider-policy-api.ts
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness/routing.md
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/model-discovery.md
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/workspace-bootstrap-files.md
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/approval-and-sandbox.md
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/app-server-transport.md
  - https://github.com/openclaw/openclaw/tree/v2026.11.7
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/tools/acp-agents/delivery.md
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/src/agents/spawn-plan.ts
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/extensions/anthropic/cli-constants.ts
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/src/agents/cli-runner/execute.ts
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/src/agents/cli-runner/execute-logging.ts
  - https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/gateway/cli-backends.md
  - https://github.com/chenhg5/cc-connect
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/platform/feishu/feishu.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config.example.toml
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/agent/codex/codex.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/agent/codex/appserver_session.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/docs/usage.md
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/Makefile
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/INSTALL.md
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/message.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/engine.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/agent/claudecode/claudecode.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/management.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config/config.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/webhook.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/bridge.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/main.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/update.go
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/instance_lock.go
  - https://github.com/chenhg5/cc-connect/tree/ba299781cb24c8839669cc0d971aff563e06c533
  - https://api.github.com/repos/chenhg5/cc-connect
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/README.md
  - https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/npm/package.json
  - https://github.com/chenhg5/cc-connect/issues/1938
  - https://github.com/modelzen/feishu-codex-bridge/blob/f4241961e17046b393323f4f46f948812f0c2194/README.md
  - https://github.com/zarazhangrui/lark-coding-agent-bridge/blob/589868119ce9a0a860aaf4936b5f9bac14e72681/README.md
  - https://www.feishu.cn/content/article/7647408304896953549
checked: 2026-11-24
---

# 飞书里驱动 Claude Code / Codex

人想要一个飞书聊天窗口，背后跑 Claude Code 或 Codex。这一页只有路径和核对过的事实，
没有选定方案；核对日期 2026-11-23。

核对的版本：

- OpenClaw：uraj 部署的 v2026.10.3（`59cb447`，2026-10-13）和核对当天的最新正式发布
  v2026.11.7（`e31bbf0`，2026-11-23 发布）。下面的行号取自 v2026.10.3；v2026.11.7 里内容
  相同的只注行号。
- cc-connect：`ba29978`（main 的头，也是 tag v1.5.4-beta.1，2026-11-23）；最新正式版
  v1.5.3（2026-11-20）。2026-10-28 的 `b2a6e6e` 上，下面核对的各项默认值相同，另注的除外。

uraj 里 OpenClaw 的部署（单个 Gateway、全部会话进沙箱、经 new-api 调模型）见
[scope-guide](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/openclaw/scope-guide.md) 和
[sandbox-guide](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/openclaw/sandbox-guide.md)，这里不重复。

## 路径一览

| 路径 | 谁收发飞书 | coding agent 跑在哪 | 核对到的限制 |
|---|---|---|---|
| OpenClaw + 原生 Codex 插件（`/codex bind`） | OpenClaw Gateway | Gateway 宿主上的 `codex app-server` | 只有直连 OpenAI 官方端点才选得上；开沙箱时 Codex 原生执行被关掉 |
| OpenClaw + ACP（`/acp spawn`） | Gateway | 宿主，不在 OpenClaw 沙箱里 | 沙箱里的会话不能发起 |
| OpenClaw + `claude-cli` 后端 | Gateway | Gateway 宿主上的 `claude` 子进程 | Gateway 环境里的 `ANTHROPIC_*` 不传给它 |
| cc-connect | cc-connect（Go 单个二进制，飞书长连接） | 同机的 `claude` / `codex` CLI | 默认值要收紧；仓库没有 LICENSE 文件 |
| 另两个桥接程序 | 见末尾 | 本机 CLI / SDK | 只读了 README |

## OpenClaw

原生 Codex 插件：

- 隐式选中 Codex 的路由只有两类：`https://api.openai.com`（路径 `/` 或 `/v1`，必须 https、
  不带端口）的 Responses 路由，和 `https://chatgpt.com/backend-api/` 下固定几条路径的 ChatGPT
  路由，
  且没有自写的 provider 请求覆盖。自定义端点（包括 new-api 这类 OpenAI 兼容中转）归
  OpenClaw 自己的运行时。
  [runtimes.md:28-34](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/providers/openai/runtimes.md#L28-L34)、
  [base-url.ts:46-57](https://github.com/openclaw/openclaw/blob/v2026.10.3/extensions/openai/base-url.ts#L46-L57)、
  [provider-policy-api.ts:204-218](https://github.com/openclaw/openclaw/blob/v2026.10.3/extensions/openai/provider-policy-api.ts#L204-L218)。
- 强制 `agentRuntime.id: "codex"` 不改变这一点，文档原话是 "does not make an incompatible
  effective route compatible"
  （[routing.md:33-34](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness/routing.md#L33-L34)）。
  [model-discovery.md:81-87](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/model-discovery.md#L81-L87)
  同样说自定义端点不自动是受支持的 Codex 路由，并警告不要为了让推理冒烟测试通过而换成自定义
  provider、去掉安全元数据或削弱审查。
- 未试的例外：文档说自定义命令、外部 app-server 连接、"custom upstream endpoints" 走另一条
  上下文通道，"OpenClaw does not reroute their inference or change their native account
  configuration"
  （[workspace-bootstrap-files.md:92-96](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/workspace-bootstrap-files.md#L92-L96)）。
  即原生 Codex 自己配的上游 OpenClaw 不碰；能不能借此接中转没有测过，文档也没说支持。
- 开了 OpenClaw 沙箱时，`codex app-server` 仍在 Gateway 宿主上跑；OpenClaw 关掉 Codex 的
  原生 Code Mode、用户 MCP 和应用插件，只给 `sandbox_exec`、`sandbox_process` 这类转进沙箱
  的工具
  （[approval-and-sandbox.md:61-67](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/approval-and-sandbox.md#L61-L67)）。
  实验选项 `appServer.experimental.sandboxExecServer` 默认 `false`
  （[app-server-transport.md:115](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/plugins/codex-harness-reference/app-server-transport.md#L115)；用法见 approval-and-sandbox.md:88-93，没试过）。
- v2026.11.7（[tag 下同路径](https://github.com/openclaw/openclaw/tree/v2026.11.7)）内容相同，行号：
  runtimes.md:28-34、routing.md:33-34、base-url.ts:66-77、provider-policy-api.ts:180-189、
  model-discovery.md:106-112、approval-and-sandbox.md:61-67、
  workspace-bootstrap-files.md:92-96。

ACP：

- 文档：ACP 会话跑在宿主运行时，"not inside the OpenClaw sandbox"；请求方会话在沙箱里时，
  `/acp spawn` 和 `sessions_spawn({ runtime: "acp" })` 都被拦；ACP 也不支持
  `sandbox: "require"`
  （[delivery.md:131-149](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/tools/acp-agents/delivery.md#L131-L149)）。
- 代码：[spawn-plan.ts:381-387](https://github.com/openclaw/openclaw/blob/v2026.10.3/src/agents/spawn-plan.ts#L381-L387) 对沙箱请求方返回
  `Sandboxed sessions cannot spawn ACP sessions because runtime="acp" runs on the host`，
  对 `sandbox="require"` 返回 `... because ACP sessions run outside the sandbox`。
- v2026.11.7 内容相同，行号：delivery.md:140-158、spawn-plan.ts:366-372。
- 全沙箱配置下起不来是按以上规则推断的，没有真发过 `/acp spawn`。

`claude-cli` 后端：

- Gateway 启动 `claude` 子进程前，先从自己继承的环境里删掉 `CLAUDE_CLI_CLEAR_ENV` 列的名字：
  `ANTHROPIC_API_KEY`、`ANTHROPIC_AUTH_TOKEN`、`ANTHROPIC_BASE_URL`、
  `ANTHROPIC_CUSTOM_HEADERS`，还有 `CLAUDE_CODE_USE_BEDROCK/VERTEX/FOUNDRY`、
  `CLAUDE_CODE_OAUTH_TOKEN`、`OTEL_EXPORTER_OTLP_*` 等。源码注释给的理由：Claude Code 先看这些
  环境变量再看本地登录，继承来的 shell 覆盖不能把 OpenClaw 管理的运行引到别的 provider；
  Claude 的配置目录照常继承，因为本地登录在那里。删完以后，OpenClaw 会把自己选定的
  `ANTHROPIC_API_KEY` 或 `CLAUDE_CODE_OAUTH_TOKEN` 再注入进去，所以“不传”只指继承来的环境。
  这个子进程也可以跑在配对的节点上，不一定在 Gateway 宿主。
  [cli-constants.ts:14-65](https://github.com/openclaw/openclaw/blob/v2026.10.3/extensions/anthropic/cli-constants.ts#L14-L65)、
  [execute.ts:378-384](https://github.com/openclaw/openclaw/blob/v2026.10.3/src/agents/cli-runner/execute.ts#L378-L384)。
- v2026.11.7：清单同在 cli-constants.ts:19-24，删除逻辑挪到 execution-env.ts:49-83。
- 文档没有给运维的开关。代码里只有一个环境变量 `OPENCLAW_LIVE_CLI_BACKEND_PRESERVE_ENV` 能
  让指定名字不被删（OpenClaw 选定了 Claude 认证时，对那几个认证变量不起作用），名字和用途都是
  live 测试
  （[execute-logging.ts:88](https://github.com/openclaw/openclaw/blob/v2026.10.3/src/agents/cli-runner/execute-logging.ts#L88)），没试过。
  cli-constants.ts:30 的注释（讲 `CLAUDE_CODE_DISABLE_1M_CONTEXT` 时）说 Claude 用户设置里的
  `env` 块优先级高于子进程环境；能否借 `settings.json` 把 `ANTHROPIC_BASE_URL` 指到中转，没有
  验证。
- 沙箱：文档说这类策略只是对 Claude Code 要跑的命令做参数级检查，"not sandboxed execution by
  OpenClaw"，cwd、PATH、环境和沙箱归 Claude Code
  （[cli-backends.md:285-288](https://github.com/openclaw/openclaw/blob/v2026.10.3/docs/gateway/cli-backends.md#L285-L288)）。

## cc-connect

上游 [chenhg5/cc-connect](https://github.com/chenhg5/cc-connect)，2026-02-28 创建，2026-11-23
有 16.4k star、441 个未关闭的 issue 和 PR。

- 飞书走官方 `oapi-sdk-go` 的 WebSocket 长连接
  （[feishu.go:36](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/platform/feishu/feishu.go#L36)），不需要公网入口。
  配了 `encrypt_key` 就改走 webhook 模式，要公网能访问的 HTTP 入口
  （[feishu.go:869-883](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/platform/feishu/feishu.go#L869-L883)）。
- 话题隔离：`thread_isolation` 默认 `false`；打开后群聊里每个话题（root message）一个 agent
  会话，私聊不分话题
  （[feishu.go:4669-4678](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/platform/feishu/feishu.go#L4669-L4678)、
  [config.example.toml:1131](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config.example.toml#L1131)）。
- Codex 后端：`backend` 默认 `exec`（每条消息一次 `codex exec`，没有审批通道）；设
  `backend = "app_server"` 才有交互审批
  （[codex.go:28-31](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/agent/codex/codex.go#L28-L31)、
  [codex.go:170-177](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/agent/codex/codex.go#L170-L177)、
  [config.example.toml:1787](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config.example.toml#L1787)）。app_server 下三个模式的映射：
  `suggest` 为 on-request + read-only，`full-auto` 为 never + workspace-write，`yolo` 为 never +
  danger-full-access
  （[appserver_session.go:464-475](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/agent/codex/appserver_session.go#L464-L475)）。
- 容器：上游没有 Dockerfile，也没有容器文档；`run_as_user` 的文档明说它不是 namespace、
  seccomp 或容器意义上的沙箱
  （[usage.md:427-428](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/docs/usage.md#L427-L428)）。发布版用 `CGO_ENABLED=0` 构建
  （[Makefile:183](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/Makefile#L183)），静态二进制放进镜像不难，但镜像里要自己装 `claude`
  和 `codex`（[INSTALL.md:76-79](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/INSTALL.md#L76-L79)）。所以“在容器里跑”由部署者搭，
  不是上游功能。

默认值：

| 项 | 核对结果 | 出处 |
|---|---|---|
| `allow_from` 为空 | 成立：空串和 `*` 都放行所有人；空串启动时只打一条 warn，`*` 连 warn 都没有；示例配置把 `"*"` 写成默认；飞书侧用同一函数检查 | [message.go:60-93](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/message.go#L60-L93)、[feishu.go:2172](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/platform/feishu/feishu.go#L2172)、[config.example.toml:1107](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config.example.toml#L1107) |
| `/mode` 切 yolo | 成立：`/mode` 不在特权命令里（特权的是 shell、show、dir、restart、upgrade、web、diff，以及 `/commands addexec`、`/cron addexec`），通过 `allow_from` 的用户都能切；Claude 的对应档是 `bypassPermissions`。特权命令由 `admin_from` 控制，为空时全部拒绝；按代码 `disabled_commands` 能屏蔽命令，没试过屏蔽 `mode` | [engine.go:1288-1296](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/engine.go#L1288-L1296)、[engine.go:1271-1285](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/engine.go#L1271-L1285)、[engine.go:1334-1352](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/engine.go#L1334-L1352)、[engine.go:7322-7330](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/engine.go#L7322-L7330)、[engine.go:10817-10877](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/engine.go#L10817-L10877)、[claudecode.go:1026](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/agent/claudecode/claudecode.go#L1026) |
| 管理 API | 部分成立：默认关闭（`enabled` 默认 `false`）；打开后监听 `:9820`（所有网卡，没有绑定地址的选项），`token` 为空时 `authenticate` 直接放行。注释写 token “required”，代码不强制。`[webhook]`（默认 9111，空 token 即不鉴权）和 `[bridge]` 同样绑 `:port`、默认关闭；`[bridge]` 的 token 为空时拒绝启动，除非设了 `insecure = true` | [management.go:206](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/management.go#L206)、[management.go:328-331](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/management.go#L328-L331)、[config.go:163-196](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config/config.go#L163-L196)、[webhook.go:68](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/webhook.go#L68)、[bridge.go:177-182](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/bridge.go#L177-L182)、[bridge.go:222-223](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/core/bridge.go#L222-L223) |
| 自动检查更新 | 成立，但只查不装：每次启动（参数解析之前）后台先请求 Gitee 上 `cg33/cc-connect` 的最新发布，失败再请求 GitHub，缓存一小时，结果只用在 `printUsage` 的提示里，不自动安装。版本号是 `dev` 或空则跳过；源码默认 `dev`，发布构建靠 ldflags 注入。没有配置项可关。安装靠 `cc-connect update` 或聊天里的 `/upgrade`（特权命令） | [main.go:241](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/main.go#L241)、[main.go:29](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/main.go#L29)、[update.go:22-25](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/update.go#L22-L25)、[update.go:46-56](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/update.go#L46-L56)、[update.go:92-98](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/update.go#L92-L98)、[main.go:1672](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/main.go#L1672) |
| 运行时改写配置 | 部分成立：`/model`、`/provider`、`/lang` 走文本级原地编辑，保留注释；添加删除 provider、自定义命令、别名、项目设置、管理 API 等走 `saveConfig`，整份配置经 TOML encoder 重写（丢注释），先写同目录临时文件再 rename。启动时还在配置文件同目录建 `.<文件名>.lock`，建不了就退出。所以配置文件和目录要可写 | [config.go:1284-1295](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config/config.go#L1284-L1295)、[config.go:1377-1382](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config/config.go#L1377-L1382)、[config.go:1630-1660](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/config/config.go#L1630-L1660)、[instance_lock.go:40-59](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/instance_lock.go#L40-L59)、[main.go:313-317](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/cmd/cc-connect/main.go#L313-L317) |
| 许可证 | 成立：仍没有 LICENSE 文件，历史里任何提交的根目录都没有过；GitHub API 的 `license` 为 `null`；README 徽章链到的 `blob/main/LICENSE` 返回 404；README 和 `npm/package.json` 自称 MIT。上游 issue #1938（2026-09-30 开）问这件事，2026-11-23 仍未关 | [仓库根目录](https://github.com/chenhg5/cc-connect/tree/ba299781cb24c8839669cc0d971aff563e06c533)、[API](https://api.github.com/repos/chenhg5/cc-connect)、[README.md:15](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/README.md#L15)、[package.json:23](https://github.com/chenhg5/cc-connect/blob/ba299781cb24c8839669cc0d971aff563e06c533/npm/package.json#L23)、[issue #1938](https://github.com/chenhg5/cc-connect/issues/1938) |

## 另两个桥接程序

只读了 README，没有读源码，也没试过：

- [modelzen/feishu-codex-bridge](https://github.com/modelzen/feishu-codex-bridge/blob/f4241961e17046b393323f4f46f948812f0c2194/README.md)（MIT，`f424196`，2026-11-18）：群对应一个固定目录，话题
  对应一个会话；后端是 Codex app-server 或 Claude Agent SDK。始终 `approvalPolicy: never`，
  沙箱是唯一闸门；“项目内只读/读写”两档由 OS 沙箱强制，只在 macOS 和原生 Windows 能强制，
  Linux 和 WSL 选这两档会拒绝启动，README 建议把后端放进容器。README 的首选流程是网页控制台
  （只绑 127.0.0.1、每次启动随机 token），但也有 `run`、`start`、`bot init` 命令行和飞书私聊菜单，
  网页控制台不是必须的。
- [zarazhangrui/lark-coding-agent-bridge](https://github.com/zarazhangrui/lark-coding-agent-bridge/blob/589868119ce9a0a860aaf4936b5f9bac14e72681/README.md)（MIT，`5898681`，2026-08-31，此后没有提交）：
  飞书官方文章
  （[Lark Coding Agent Bridge](https://www.feishu.cn/content/article/7647408304896953549)）介绍的
  就是它。转发到本机的 `claude` 或 `codex`；每个 chat、话题、文档评论串一个会话。默认只有应用
  创建者能用；权限默认 `full`，即 Claude `bypassPermissions`、Codex `danger-full-access`；工作目录
  不是沙箱。

## 没有核对的

- 没有实际部署过 cc-connect、feishu-codex-bridge 或 lark-coding-agent-bridge，上面都是读
  代码和 README 的结果；cc-connect 的 issue 里的运行问题（假死、重复消息等）没有逐条核对。
- OpenClaw 的各条路径没有在飞书里实际跑过；“全沙箱下 ACP 起不来”“claude-cli 用
  `settings.json` 指中转”“原生 Codex 接中转的例外路径”都是代码或文档推断，未验证。
