# skills

把全局 skill、它们要的 hook 和人的偏好装给 Claude Code 和 Codex。

- **全局 skill**：来自一个或多个来源仓库的 `skills/<name>/`（里面有 `SKILL.md`）。
  每个来源有一份只跟 `main` 的检出，所以人在自己的工作副本里切分支，不会改变 agent
  读到的 skill。来源都是策略仓库，现在只有一个：agent-policy；来源列表在
  `src/python/pylon/skills.py` 的 `SOURCES`。
- **hook**：全局 skill 要工具在开工、压缩等时候跑它的脚本，就在自己目录里放一个
  `hooks.json`，格式和 Claude Code 配置里的 `"hooks"` 一样，Codex 的 `hooks.json` 也是这个
  格式。命令通过 `~/.agents/skills/<name>/` 跑脚本，两个工具都有这个链接。这个模块把它合并进
  `~/.claude/settings.json`：用户级的 hook 只能写在这个文件里。Codex 还没接：它要人在
  `/hooks` 里逐个信任、hook 一改就要重新信任，而且现在唯一要 hook 的 session-notes 有一半
  （压缩前提醒）在 Codex 里不起作用。
- **偏好**：策略仓库里的 `preferences.md`，链接成各工具的全局说明文件，每个 session
  都会加载。见 [agent-policy](projects/261120-agent-policy/README.md)。
- **项目 skill** 不归这个模块管：只在一个仓库里用的 skill 放在那个仓库的
  `.claude/skills/`、`.agents/skills/`，工具自己会读。

级别：能管。

## 用法

```sh
bin/pylon-skills update   # 每个来源：第一次克隆，之后快进到 origin/main；再建好链接、写好 hook
bin/pylon-skills status   # 各检出在哪个提交，各工具目录里有哪些链接，各配置文件里有哪些 skill 的 hook
bin/pylon-skills remove   # 删掉所有链接（包括全局说明文件）和装的 hook，检出保留
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
  - Claude Code 的配置：`~/.claude/settings.json`，和旁边所有 `settings.json.*` 变体。人用
    `settings.json` 链到其中一个来切换，所以链接本身不动，只顺着链接改真正的文件。只增删
    这个模块装的 hook：命令里跑的是 `~/.agents/skills/<name>/` 下的代码，而 `<name>` 是这个
    模块的链接，或者已经不存在（skill 删了，hook 跟着删）。别的 hook 和设置都不动。
    skill 声明的 hook，命令里要出现 `~/.agents/skills/<name>/`，模块靠它认 hook 归哪个 skill；
    这不算安全检查，人合并来源仓库的 PR 才是那一道。人手动贴进配置、跑这些 skill 代码的
    hook 也算这个模块的，`update` 会删掉，要加就写进 skill 的 `hooks.json`。
  - 内容有变才写：先把原文件复制到
    `~/.local/state/pylon/settings-backup/<时间>/<原文件的绝对路径>`，再写临时文件、改名替换，
    中途出错不会留下半个文件。重写后的 JSON 统一 2 格缩进，键的顺序不变。`<时间>` 目录
    是 0700，但里面的副本沿用原文件的权限（现在 644）；副本不会自动删。
  - 写不了的文件（只读、在 `/gnu/store` 里）、读不懂的文件、断掉的链接：跳过并报错；写不了
    的会打出它应有的 `"hooks"`，人可以手贴。
- **每个 session 都会读到什么**：全局 skill 的名字和描述，以及偏好全文。所以来源
  仓库的 `main` 上有什么，所有 agent 都会看到、照着做。来源仓库的改动要经人批准合并。
- **每个 session 都会跑什么**：skill 声明的 hook。`update` 之后，来源仓库 `main` 上的这些
  脚本在每个 Claude Code session 开工、压缩等时候自动运行，不经过 agent，用人的权限。
  审批也是那一道：人合并来源仓库的 PR。Claude Code 文档说会自己发现配置文件变了，已经开着的
  session 应该也会用上新的 hook，没实测；这里换文件是改名替换 `settings.json` 链接指向的
  那个文件，文档没说文件监视会不会看到。
- **网络**：`update` 用 `git` 通过 HTTPS 访问 `github.com`，只读。agent-policy 是私有的，
  靠 `gh` 配好的 git 凭据（`gh auth setup-git`）。
- **权限和秘密**：不用 sudo。会读、改上面那些配置文件，里面可能有 `env`（token 之类）；
  副本留在备份目录里，见上。
- **坏了会怎样**：所有 agent 新开的 session 同时受影响；已经开着的 session 不受影响，
  hook 除外，见上。
- **怎么关掉**：`bin/pylon-skills remove` 删掉所有链接和装的 hook；配置文件改坏了，把
  备份目录里的文件拷回原路径；要回到旧版本，在检出里
  `git checkout <提交>`，下次 `update` 会因为不在 `main` 而停下，直到人切回 `main`。
- **容器**：容器里要只读挂载 `~/.local/share/agent-policies`、`~/.agents`，用 Codex 的话还有
  `~/.codex`，否则链接是断的。

## 为什么不经 Guix

reconfigure 慢，skill 和偏好要合并后马上生效，所以直接用只跟 `main` 的 git 检出。
备选是单独一个 Guix profile，里面只装 skill：比 reconfigure 快，有代数能回退，
容器共享 store 时直接可见；代价是每次更新都要过一遍 Guix。容器需要时再看。
