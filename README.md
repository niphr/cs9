# cs9 <a href="https://niphr.github.io/cs9/"><img src="man/figures/logo.png" align="right" width="120" /></a>

[![CRAN status](https://www.r-pkg.org/badges/version/cs9)](https://cran.r-project.org/package=cs9)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/cs9)](https://cran.r-project.org/package=cs9)

## Overview 

[Core Surveillance 9](https://niphr.github.io/cs9/) ("cs9") is a free and open-source framework for real-time analysis and disease surveillance.

Read the introduction vignette [here](https://niphr.github.io/cs9/articles/cs9.html) or run `help(package="cs9")`.

## Databases

cs9 stores its tables in PostgreSQL or in SQLite, chosen by the `CS9_DBCONFIG_DRIVER` environment
variable. SQLite needs no server: set `CS9_DBCONFIG_DRIVER=SQLite` and give each access level a
file path in `CS9_DBCONFIG_DB_<ACCESS>`.

- [Installation](https://niphr.github.io/cs9/articles/installation.html) — start on SQLite, then
  the PostgreSQL setup.
- [Database backends](https://niphr.github.io/cs9/articles/backends.html) — the two environments
  side by side, and which variable each backend reads.
