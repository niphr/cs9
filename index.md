---
title: cs9
---

<p class="cs-section">How it works</p>

<div class="cs-cards">
<div class="cs-card"><div class="cs-card-num">01</div><h3>Surveillance system</h3><p>A single <code>SurveillanceSystem_v9</code> R6 object registers all database tables and tasks. It gives the whole pipeline one place to configure and run.</p></div>
<div class="cs-card"><div class="cs-card-num">02</div><h3>Tasks, plans, analyses</h3><p>CS9 arranges work in a three-level hierarchy — tasks contain plans, plans contain analyses. CS9 pulls data once per plan and reuses it across all analyses inside that plan. Parallel execution across plans is optional.</p></div>
<div class="cs-card"><div class="cs-card-num">03</div><h3>Execution logging</h3><p><code>update_config_log()</code> records every task run. <code>get_config_tasks_stats()</code> returns timing and status, so you can diagnose failures and track pipeline performance over time.</p></div>
</div>
