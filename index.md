How it works

01

### Surveillance system

A single `SurveillanceSystem_v9` R6 object registers all database tables
and tasks. It gives the whole pipeline one place to configure and run.

02

### Tasks, plans, analyses

CS9 arranges work in a three-level hierarchy — tasks contain plans,
plans contain analyses. CS9 pulls data once per plan and reuses it
across all analyses inside that plan. Parallel execution across plans is
optional.

03

### Execution logging

[`update_config_log()`](https://niphr.github.io/cs9/reference/update_config_log.md)
records every task run.
[`get_config_tasks_stats()`](https://niphr.github.io/cs9/reference/get_config_tasks_stats.md)
returns timing and status, so you can diagnose failures and track
pipeline performance over time.
