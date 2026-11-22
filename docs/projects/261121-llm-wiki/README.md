# llm-wiki

## 目标

定下 workshop 的 `llm-wiki/` 怎么用：里面怎么组织，agent 什么时候读、什么时候写，
怎么防止内容过时。定完之后，agent 做调研前会先查它，调研完会把结论放进去，
同一件事不用每个 session 重新查一遍。

## 背景

workshop skill 只定了底线：每个 workshop 可以有 `llm-wiki/`，放 agent 关心、人不关心
的知识和素材；它进 git，所以公开 workshop 里只放能公开的；文档只能在末尾的参考一节
引用它；`src` 和 `env` 不引用它；内部结构暂时自由。

之前在人的一个 space 下试过一个 llm-wiki，没有成形的设计，只积累了一篇调研。另外还有
两篇 agent 调研没有合并。这三篇等设计定下后再搬进来，作为第一批内容。

## 待定

暂无。

## 决定

- 11/21：开这个项目，项目名叫 llm-wiki；已有的三篇素材等设计定下后作为第一批内容搬进来。
- 11/22：目录叫 `llm-wiki/`，不加点，agent 搜得到比人搜索时不被干扰更重要（agent-policy #7）。
- 11/22：分层、读写和整理的时机、边界，见 [llm-wiki-design](llm-wiki-design.md)。
- 11/22：用 hook 做两件事：压缩后重新给一次索引；压缩前让压缩摘要保留还没写进 llm-wiki
  的外部知识（第 2 步验证效果）。
- 11/22：三篇素材放进 pylon 的 `llm-wiki/`。
- 11/23：hook 先配在各 workshop 自己的仓库里，现在只有 pylon；第二个 workshop 有了
  llm-wiki 再看要不要改成全局配置。
- 11/22：后台反思由定时器触发，不绑在 session 上，放到 reflection 项目里做。

## 文档

- [TODO](TODO.md)：进度。
- [llm-wiki-design](llm-wiki-design.md)：llm-wiki 分几层、每层放什么、什么时候读写。

## 参考

- [agent-memory](../../../llm-wiki/agent-memory.md)：Karpathy、Letta、
  Hermes、OpenClaw 怎么管 agent 的记忆。
- [agent-hooks](../../../llm-wiki/agent-hooks.md)：Claude Code 和 Codex 的 hook。
