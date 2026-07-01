How it works

01

### Surveillance system

A single `SurveillanceSystem_v9` R6 object registers all database tables
and tasks, giving the whole pipeline one place to configure and run.

02

### Tasks, plans, analyses

Work is arranged in a three-level hierarchy — tasks contain plans, plans
contain analyses — so data is pulled once per plan and reused across all
analyses inside it, with optional parallel execution across plans.

03

### Execution logging

Every task run is recorded via
[`update_config_log()`](https://niphr.github.io/cs9/reference/update_config_log.md),
and
[`get_config_tasks_stats()`](https://niphr.github.io/cs9/reference/get_config_tasks_stats.md)
returns timing and status so you can diagnose failures and track
pipeline performance over time.
