---
title: cs9
---

<p class="cs-section">How it works</p>

<div class="cs-cards">
<div class="cs-card"><div class="cs-card-num">01</div><h3>Surveillance system</h3><p>A single <code>SurveillanceSystem_v9</code> R6 object registers all database tables and tasks, giving the whole pipeline one place to configure and run.</p></div>
<div class="cs-card"><div class="cs-card-num">02</div><h3>Tasks, plans, analyses</h3><p>Work is arranged in a three-level hierarchy — tasks contain plans, plans contain analyses — so data is pulled once per plan and reused across all analyses inside it, with optional parallel execution across plans.</p></div>
<div class="cs-card"><div class="cs-card-num">03</div><h3>Execution logging</h3><p>Every task run is recorded via <code>update_config_log()</code>, and <code>get_config_tasks_stats()</code> returns timing and status so you can diagnose failures and track pipeline performance over time.</p></div>
</div>

## Overview 

[Core Surveillance 9](https://niphr.github.io/cs9/) ("cs9") is a free and open-source framework for real-time analysis and disease surveillance.

Read the introduction vignette [here](https://niphr.github.io/cs9/articles/cs9.html) or run `help(package="cs9")`.
