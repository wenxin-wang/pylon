# skills

把 pylon `skills/` 下的 agent skill 装给 Claude Code 和 Codex。装的是一份只跟 `main`
的 pylon 检出，所以人在 `~/src/pylon` 里切分支，不会改变 agent 读到的 skill。
为什么这样装，见 [workshop-design](projects/261120-workshop-design/README.md) 的决定。

级别：能管。

## 用法

```sh
bin/pylon-skills update   # 第一次克隆检出，之后快进到 origin/main；再建好链接
bin/pylon-skills status   # 检出在哪个提交，各工具目录里有哪些链接
bin/pylon-skills remove   # 删掉所有链接，检出保留
```

从任何一份 pylon 检出运行都行，链接总是指向 `~/.local/share/pylon/main`。
新 skill 合并到 `main` 后，跑一次 `update`；agent 新开的 session 才会读到。

一个 skill 是 `skills/<name>/` 目录，里面必须有 `SKILL.md`。

测试：

```sh
PYTHONPATH=src/python python3 -m unittest discover -s test/python/pylon
```

## 影响范围

- **写哪些文件**：
  - `~/.local/share/pylon/main`：pylon 的检出，只在 `main` 上快进。检出不在 `main`
    或有本地改动时，`update` 报错退出，不动它。
  - `~/.claude/skills/<name>`、`~/.agents/skills/<name>`：符号链接，指向检出里的
    `skills/<name>`。同名的东西如果不是这个模块建的链接，就跳过并报错，不覆盖；
    `skills/` 里删掉的 skill，对应的链接会删掉。
- **网络**：`update` 用 `git` 访问 `github.com`，只读。
- **权限和秘密**：没有。不用 sudo，不读秘密。
- **坏了会怎样**：所有 agent 新开的 session 都读同一份 skill，`main` 上的一个坏
  skill 会同时影响所有 workshop。已经开着的 session 不受影响。
- **怎么关掉**：`bin/pylon-skills remove` 删掉所有链接；要回到旧版本，在检出里
  `git checkout <提交>`，下次 `update` 会因为不在 `main` 而停下，直到人切回 `main`。
- **容器**：现在的 contained-agent 容器只读挂载了 `~/.claude`，没有挂
  `~/.local/share/pylon` 和 `~/.agents`，所以容器里的链接是断的。要人在 uraj 的
  contained-agent 配置里加这两个只读挂载。
