# llm-wiki 进度

状态：已结束（11/23），已归档。

1. ~~写进 skill~~ 已完成。workshop skill 的 llm-wiki 一节和索引脚本
   `skills/workshop/scripts/llm_wiki_index.py`（agent-policy PR #8、#9，三轮审查）。
2. ~~搬素材、接 hook~~ 已完成。三篇素材搬进 pylon 的 `llm-wiki/`（PR #14）；hook 配在
   pylon 仓库里（PR #15）。Claude Code 上人验证过：开工和压缩后都拿到索引，压缩前提醒
   有效。Codex 没验证成：contained-agent 退出时丢掉对配置的修改，项目信任存不下来，
   项目 hook 就不加载。挪到 contained-agent-v2，见 uraj-migration 的 TODO。
