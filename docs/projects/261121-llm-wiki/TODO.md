# llm-wiki 进度

状态：进行中。

当前：第 2 步。

1. ~~写进 skill~~ 已完成。workshop skill 的 llm-wiki 一节和索引脚本
   `skills/workshop/scripts/llm_wiki_index.py`（agent-policy PR #8、#9，三轮审查）。
2. 搬素材、接 hook：三篇素材按新结构搬进 pylon 的 `llm-wiki/`（PR #14）；hook 配在
   pylon 仓库里：Claude Code 的 `.claude/settings.json`（开工和压缩后给索引、压缩前提醒），
   Codex 的 `.codex/config.toml`（只给索引，项目要设为信任）。还差：在 contained-agent
   里新开一个 session，确认索引进了上下文；压缩前提醒等第一次压缩时看效果。
