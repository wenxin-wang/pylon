# contained-agent-v2 进度

状态：进行中：按节迁设计，边迁边讨论。下一节：设备。

当前：第 1 步。

uraj 里已做完的原型和实测（11/15）见 README 的“文档”一节，结论在
[box-design](box-design.md)。

1. 迁设计并讨论：把 uraj 的 [v2 README][uraj-v2] 按节迁进 [box-design](box-design.md)，
   一节一个 PR，迁到哪节就讨论哪节。进行中。已迁：盒子怎么起、网络、文件怎么进盒子、
   访问盒子里的服务、盒子策略、运行中改授权。还剩：设备、Paseo 接盒子
   （路线 C）、agent 的 GitHub 身份、建盒子和删盒子、不用 Paseo 时怎么进盒子。已知的坑
   见 [step-1-handoff](step-1-handoff.md)。
2. 盒子边界清单：列出 v1 agent 容器的挂载、synthetic HOME、凭据和设备，逐项决定在盒子里
   怎么实现。
3. agent 进盒子：按路线 C 在盒子里起 agent。

[uraj-v2]: https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/README.md
