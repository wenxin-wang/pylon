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

- **用 hook 借鉴别家的哪些做法。** 调研见文末参考。
  - 压缩后重新给索引：Claude Code 和 Codex 的 SessionStart 都能匹配“压缩后”这个来源，
    同一个脚本再跑一次。代价几乎为零。建议做。
  - 压缩前提醒（借 OpenClaw 的 memory flush）：Claude Code 的 PreCompact 输出只能引导
    压缩摘要，不能让 agent 先动手写文件；可以让摘要保留“学到了、还没写进 llm-wiki 的
    外部知识”。代价：一个 hook。建议做，第 2 步验证效果。
- **三篇素材放进哪个 workshop。** 第 2 步再定：内容能公开的放 pylon，否则等私有的
  workshop。

## 决定

- 11/21：开这个项目，项目名叫 llm-wiki；已有的三篇素材等设计定下后作为第一批内容搬进来。
- 11/22：目录叫 `llm-wiki/`，不加点，agent 搜得到比人搜索时不被干扰更重要（agent-policy #7）。
- 11/22：分层、读写和整理的时机、边界，见 [llm-wiki-design](llm-wiki-design.md)。
- 11/22：后台反思由定时器触发，不绑在 session 上，放到 reflection 项目里做。

## 文档

- [TODO](TODO.md)：进度。
- [llm-wiki-design](llm-wiki-design.md)：llm-wiki 分几层、每层放什么、什么时候读写。

## 参考

- [agent-memory](../../../llm-wiki/agent-memory.md)：Karpathy、Letta、
  Hermes、OpenClaw 怎么管 agent 的记忆。
- [agent-hooks](../../../llm-wiki/agent-hooks.md)：Claude Code 和 Codex 的 hook。
