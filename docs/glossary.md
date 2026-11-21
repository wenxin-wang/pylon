# 术语表

一个东西只有一个名字，一个名字只指一个东西。新词先在这里登记再用；
编号前必须带语义名。中文文档用中文名，英文文档用英文名，二者都登记。
代码标识符跟着术语走。

## 工作方式

| 术语 | 含义 |
|---|---|
| 人（the human） | 本仓库的主人，审阅并合并 PR。文档里不用“我”指人，也不用“我”指 agent |
| agent | 写代码和文档的 AI，不论哪个工具、哪个 session |
| space | 人自己写的笔记本（org mode）。agent 只读，不写。例：garden、sketchpad |
| workshop | 由 agent 推进、人审阅的仓库，按本仓库 `AGENTS.md` 这套约定运作。有没有代码都算。pylon 是一个 workshop |
| wiki | workshop 下的 `wiki/`，放 agent 关心、人不关心的知识和素材 |
| 策略仓库（policy repo） | 私有的 `agent-policy` 仓库：全局的常驻指令和每个 workshop 的策略。只放数据，不是 workshop；格式由 pylon 里读它的代码定义 |
| 模块表（module table） | `README.md` 里的模块表：有哪些模块、级别、依赖、入口。上限一页。改它要人点头 |
| 模块（module） | 模块表上登记的一块功能；也是 `docs/<module>.md` 的名字和 PR topic |
| 级别（level） | 人需要懂一个模块到什么程度：能用、能管、能讲。顺着依赖往下传 |
| 能用（use） | 知道它存在、干什么、怎么用 |
| 能管（operate） | 还知道它碰什么、坏了会怎样、怎么关掉 |
| 能讲（explain） | 懂设计本身和关键决定 |
| 影响范围（blast radius） | 一个模块碰到的权限、秘密、网络和主机，坏了会怎样，怎么关掉。写在 `docs/<module>.md` |
| 子项目（project） | `docs/projects/<yymmdd>-<project>/` 下推进的一件事 |
| 状态（status） | 子项目 `TODO.md` 标题下的第一行，三选一：进行中；等人（要定什么）；挂起（原因） |
| 步（step） | 子项目 `TODO.md` 里的一条，带编号和语义名 |
| 交接（handoff） | `step-<n>-handoff.md`，第 n 步交给下一个 session 的文档 |
| 活文档（living doc） | 描述现状、随时改的文档：术语表、模块入口、guide |
| 用法手册（guide） | `docs/<module>/<topic>-guide.md`，不带日期，描述 `main` 上的现状 |
| 设计文档（design doc） | 子项目里的 `<topic>-design.md`，项目进行中可以改，随项目归档 |
| 记录（record） | 子项目里的 `<topic>-<kind>-<yyyymmdd>.md`，一次发生过的事，写完不改 |

## 仓库

| 术语 | 含义 |
|---|---|
| pylon | 本仓库，公开的 workshop |
| uraj | `wenxin-wang/uraj`，pylon 的前身，模块逐个迁到 pylon 后退役 |
| garden | `wenxin-wang/garden`，人的公开笔记和主页（原名 pylon），一个 space |
