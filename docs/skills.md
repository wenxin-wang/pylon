# skills

把全局 skill 和人的偏好装给 Claude Code 和 Codex。

- **全局 skill**：来自一个或多个来源仓库的 `skills/<name>/`（里面有 `SKILL.md`）。
  每个来源有一份只跟 `main` 的检出，所以人在自己的工作副本里切分支，不会改变 agent
  读到的 skill。来源都是策略仓库，现在只有一个：agent-policy；来源列表在
  `src/python/pylon/skills.py` 的 `SOURCES`。
- **偏好**：策略仓库里的 `preferences.md`，链接成各工具的全局说明文件，每个 session
  都会加载。见 [agent-policy](projects/261120-agent-policy/README.md)。
- **项目 skill** 不归这个模块管：只在一个仓库里用的 skill 放在那个仓库的
  `.claude/skills/`、`.agents/skills/`，工具自己会读。

级别：能管。

## 用法

```sh
bin/pylon-skills update   # 每个来源：第一次克隆，之后快进到 origin/main；再建好链接
bin/pylon-skills status   # 各检出在哪个提交，各工具目录里有哪些链接
bin/pylon-skills remove   # 删掉所有链接（包括全局说明文件），检出保留
```

从任何一份 pylon 检出运行都行，链接总是指向 `~/.local/share/agent-policies/<来源>`。
来源合并新的 skill 后跑一次 `update`；agent 新开的 session 才会读到。

测试：

```sh
PYTHONPATH=src/python python3 -m unittest discover -s test/python/pylon
```

## 影响范围

- **写哪些文件**：
  - `~/.local/share/agent-policies/<来源>`：每个来源的检出，只在 `main` 上快进。检出不在
    `main` 或有本地改动时，`update` 报错退出，不动它。
  - `~/.claude/skills/<name>`、`~/.agents/skills/<name>`：符号链接，指向某个检出里的
    `skills/<name>`。同名的东西如果不是这个模块建的链接，就跳过并报错，不覆盖；
    两个来源有同名 skill 时，两边都不链接，报错；来源里删掉的 skill，链接也删掉。
  - `~/.claude/CLAUDE.md`、`~/.codex/AGENTS.md`：符号链接，指向策略仓库的
    `preferences.md`。已有的文件不覆盖，跳过并报错；`preferences.md` 删掉后链接也删掉。
- **每个 session 都会读到什么**：全局 skill 的名字和描述，以及偏好全文。所以来源
  仓库的 `main` 上有什么，所有 agent 都会看到、照着做。来源仓库的改动要经人批准合并。
- **网络**：`update` 用 `git` 访问 `github.com`，只读。agent-policy 是私有的，走 SSH，
  用人的 SSH 密钥。
- **权限和秘密**：没有。不用 sudo，不读秘密。
- **坏了会怎样**：所有 agent 新开的 session 同时受影响；已经开着的 session 不受影响。
- **怎么关掉**：`bin/pylon-skills remove` 删掉所有链接；要回到旧版本，在检出里
  `git checkout <提交>`，下次 `update` 会因为不在 `main` 而停下，直到人切回 `main`。
- **容器**：容器里要只读挂载 `~/.local/share/agent-policies`、`~/.agents`，用 Codex 的话还有
  `~/.codex`，否则链接是断的。
