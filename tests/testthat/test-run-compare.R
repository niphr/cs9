# compare_staged_to_live() and check_staged_against_live() compare the rows that
# a task run staged with the live rows, during the run. These tests stage rows
# in a real Task$run() on SQLite, with a plain table and with a partitioned
# table of 3 partitions. The run fixtures are in helper-run-staging.R.
#
# The table has the columns x, g and n. The primary key is x and g, plus part
# for the partitioned table. Each test compares on the key x and the value n,
# so several rows of one partition can share a key. Partition p3 never holds a
# row.

new_compare_out <- function(dbconfig, kind, envir = parent.frame()) {
  fields <- c(x = "TEXT", g = "TEXT", n = "DOUBLE")
  if (kind == "plain") {
    out <- DBTableExtended_v9$new(
      dbconfig = dbconfig,
      table_name = "anon_cmp_plain",
      field_types = fields,
      keys = c("x", "g"),
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
    )
  } else {
    out <- DBPartitionedTableExtended_v9$new(
      dbconfig = dbconfig,
      table_name_base = "anon_cmp_part",
      table_name_partitions = c("p1", "p2", "p3"),
      column_name_partition = "part",
      field_types = fields,
      keys = c("x", "g", "part"),
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
    )
  }
  withr::defer(out$disconnect(), envir = envir)
  return(out)
}

# Rows as a data.table. `part` is dropped for the plain table.
cmp_rows <- function(kind, x, g, n, part = NA_character_) {
  d <- data.table::data.table(x = x, g = g, n = as.numeric(n), part = part)
  if (kind == "plain") {
    d[, part := NULL]
  }
  return(d)
}

# Writes the live rows with DBI, outside any task run. Each partition table
# that holds a row exists. The others do not.
seed_cmp_live <- function(out, rows) {
  rows <- data.table::copy(rows)
  rows[, auto_last_updated_datetime := "2026-01-01 00:00:00"]
  partitioned <- inherits(out, "DBPartitionedTableExtended_v9")
  db <- if (partitioned) out$tables[[1]]$dbconfig$db else out$dbconfig$db
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!partitioned) {
    suppressMessages(invisible(out$tbl()))
    DBI::dbAppendTable(con, out$table_name, as.data.frame(rows))
    return(invisible(NULL))
  }
  for (p in unique(rows$part)) {
    t <- out$tables[[p]]
    suppressMessages(invisible(t$tbl()))
    DBI::dbAppendTable(con, t$table_name, as.data.frame(rows[part == p]))
  }
  return(invisible(NULL))
}

# Runs a task of 2 plans on `out`. Plan 1 empties the table and stages
# `staged`, so the publish replaces the live rows. Plan 2, during the
# run, returns fn(tables$out), or the error it gives.
in_run <- function(out, staged, fn, envir = parent.frame()) {
  ctl <- local_stage_ctl(envir = envir)
  ctl$rows <- function(plan) {
    if (plan == 1) {
      return(data.table::copy(staged))
    }
    return(NULL)
  }
  ctl$hooks[["2"]] <- function(tables, argset) {
    ctl$got <- tryCatch(fn(tables$out), error = function(e) e)
    return(invisible(NULL))
  }
  err <- run_attempt(new_stage_task(out, n_plans = 2), run_key = "compare")
  testthat::expect_null(err)
  return(ctl$got)
}

# The comparison of x and n during a run that stages `staged`, after `live`
# was written.
compare_in_run <- function(
  kind,
  live,
  staged,
  filter = NULL,
  envir = parent.frame()
) {
  db <- local_staging_db(envir = envir)
  out <- new_compare_out(db, kind, envir = envir)
  seed_cmp_live(out, live)
  return(in_run(
    out,
    staged,
    function(tb) compare_staged_to_live(tb, "x", "n", filter),
    envir = envir
  ))
}

expected_comparison <- function(x, partition, staged, live) {
  return(data.table::data.table(
    x = x,
    partition = partition,
    staged = as.numeric(staged),
    live = as.numeric(live)
  ))
}

# The failures of a check, or NULL when it passes.
check_failures <- function(
  comparison,
  rules = c("partitions_agree", "no_missing", "no_decrease")
) {
  err <- tryCatch(
    {
      check_staged_against_live(comparison, rules)
      NULL
    },
    cs9_staged_check_error = function(e) e
  )
  if (is.null(err)) {
    return(NULL)
  }
  return(as.data.frame(err$failures))
}

failure <- function(x, problem) {
  return(data.frame(x = x, problem = problem))
}

all_rules <- c("partitions_agree", "no_missing", "no_decrease")

test_that("the comparison of a plain table has one row per key and value pair", {
  live <- cmp_rows("plain", c("a", "a", "b"), c("t", "u", "t"), c(10, 1, 20))
  staged <- cmp_rows("plain", c("a", "c"), c("t", "t"), c(12, 3))

  got <- compare_in_run("plain", live, staged)

  # Key a has the staged value 12 and the live values 1 and 10.
  expect_equal(
    got,
    expected_comparison(
      x = c("a", "a", "b", "c"),
      partition = NA_character_,
      staged = c(12, 12, NA, 3),
      live = c(1, 10, 20, NA)
    )
  )
})

test_that("the filter of a plain table applies to the staged and the live rows", {
  live <- cmp_rows("plain", c("a", "a", "b"), c("t", "u", "t"), c(10, 1, 20))
  staged <- cmp_rows("plain", c("a", "a", "c"), c("t", "u", "t"), c(12, 2, 3))

  got <- compare_in_run(
    "plain",
    live,
    staged,
    filter = function(x) dplyr::filter(x, g == "t")
  )

  expect_equal(
    got,
    expected_comparison(
      x = c("a", "b", "c"),
      partition = NA_character_,
      staged = c(12, NA, 3),
      live = c(10, 20, NA)
    )
  )
})

test_that("the comparison of a partitioned table has one row per key and partition", {
  live <- cmp_rows(
    "partitioned",
    c("a", "a", "b", "a", "d"),
    c("t", "u", "t", "t", "t"),
    c(10, 1, 20, 10, 7),
    c("p1", "p1", "p1", "p2", "p2")
  )
  staged <- cmp_rows(
    "partitioned",
    c("a", "a", "c"),
    c("t", "t", "t"),
    c(12, 12, 3),
    c("p1", "p2", "p2")
  )

  got <- compare_in_run("partitioned", live, staged)

  expect_equal(
    got,
    expected_comparison(
      x = c("a", "a", "a", "b", "c", "d"),
      partition = c("p1", "p1", "p2", "p1", "p2", "p2"),
      staged = c(12, 12, 12, NA, 3, NA),
      live = c(1, 10, 10, 20, NA, 7)
    )
  )
})

test_that("the filter of a partitioned table applies to the staged and to each live partition", {
  live <- cmp_rows(
    "partitioned",
    c("a", "a", "b", "a", "a", "d"),
    c("t", "u", "t", "t", "u", "t"),
    c(10, 1, 20, 10, 2, 7),
    c("p1", "p1", "p1", "p2", "p2", "p2")
  )
  staged <- cmp_rows(
    "partitioned",
    c("a", "a", "a", "c"),
    c("t", "t", "u", "t"),
    c(12, 12, 5, 3),
    c("p1", "p2", "p2", "p2")
  )

  got <- compare_in_run(
    "partitioned",
    live,
    staged,
    filter = function(x) dplyr::filter(x, g == "t")
  )

  expect_equal(
    got,
    expected_comparison(
      x = c("a", "a", "b", "c", "d"),
      partition = c("p1", "p2", "p1", "p2", "p2"),
      staged = c(12, 12, NA, 3, NA),
      live = c(10, 10, 20, NA, 7)
    )
  )
})

for (kind in c("plain", "partitioned")) {
  # The partitioned rows go to p1 and p2. The plain rows have one g per
  # partition of the partitioned rows.
  rows <- function(x, n, where) {
    g <- if (kind == "plain") where else rep("t", length(x))
    return(cmp_rows(kind, x, g, n, where))
  }

  test_that(paste0("each rule passes when no key fails (", kind, ")"), {
    live <- rows(
      c("a", "a", "b", "b"),
      c(10, 10, 5, 5),
      c("p1", "p2", "p1", "p2")
    )
    staged <- rows(
      c("a", "a", "b", "b", "c", "c"),
      c(12, 12, 5, 5, 3, 3),
      c("p1", "p2", "p1", "p2", "p1", "p2")
    )

    got <- compare_in_run(kind, live, staged)

    for (rule in all_rules) {
      expect_null(check_failures(got, rule), label = rule)
    }
    res <- withVisible(check_staged_against_live(got))
    expect_false(res$visible)
    expect_identical(res$value, got)
  })

  test_that(
    paste0(
      "no_decrease fails a staged value below the live value (",
      kind,
      ")"
    ),
    {
      live <- rows(c("a", "a"), c(10, 10), c("p1", "p2"))
      staged <- rows(c("a", "a"), c(9, 9), c("p1", "p2"))

      got <- compare_in_run(kind, live, staged)

      expect_equal(
        check_failures(got, "no_decrease"),
        failure("a", "staged 9, stored 10")
      )
      expect_equal(check_failures(got), failure("a", "staged 9, stored 10"))
    }
  )

  test_that(
    paste0(
      "no_decrease passes a staged value equal to the live value (",
      kind,
      ")"
    ),
    {
      live <- rows(c("a", "a"), c(10.5, 10.5), c("p1", "p2"))
      staged <- rows(c("a", "a"), c(10.5, 10.5), c("p1", "p2"))

      got <- compare_in_run(kind, live, staged)

      expect_null(check_failures(got, "no_decrease"))
      expect_null(check_failures(got))
    }
  )

  test_that(
    paste0(
      "partitions_agree fails when the live partitions disagree (",
      kind,
      ")"
    ),
    {
      live <- rows(c("a", "a"), c(10, 11), c("p1", "p2"))
      staged <- rows(c("a", "a"), c(11, 11), c("p1", "p2"))

      got <- compare_in_run(kind, live, staged)

      expected <- failure(
        "a",
        "the live partitions disagree on the stored total (10, 11)"
      )
      expect_equal(check_failures(got, "partitions_agree"), expected)
      expect_equal(check_failures(got), expected)
    }
  )

  test_that(
    paste0(
      "partitions_agree fails when the staged partitions disagree (",
      kind,
      ")"
    ),
    {
      live <- rows(c("a", "a"), c(10, 10), c("p1", "p2"))
      staged <- rows(c("a", "a"), c(12, 13), c("p1", "p2"))

      got <- compare_in_run(kind, live, staged)

      expected <- failure(
        "a",
        "the partitions disagree on the staged total (12, 13)"
      )
      expect_equal(check_failures(got, "partitions_agree"), expected)
      expect_equal(check_failures(got), expected)
    }
  )

  test_that(
    paste0(
      "no_missing fails a key that one live partition holds and no staged row holds (",
      kind,
      ")"
    ),
    {
      live <- rows(c("a", "b", "b"), c(10, 4, 4), c("p1", "p1", "p2"))
      staged <- rows(c("b", "b"), c(4, 4), c("p1", "p2"))

      got <- compare_in_run(kind, live, staged)

      expected <- failure("a", "staged none, stored 10")
      expect_equal(check_failures(got, "no_missing"), expected)
      expect_equal(check_failures(got), expected)
      expect_null(check_failures(got, c("partitions_agree", "no_decrease")))
    }
  )

  test_that(
    paste0("a key that no live partition holds passes every rule (", kind, ")"),
    {
      live <- rows(c("b", "b"), c(4, 4), c("p1", "p2"))
      staged <- rows(
        c("b", "b", "c", "c"),
        c(4, 4, 3, 3),
        c("p1", "p2", "p1", "p2")
      )

      got <- compare_in_run(kind, live, staged)

      # Key c has staged rows and no live row.
      expect_gt(sum(got$x == "c"), 0)
      expect_true(all(is.na(got$live[got$x == "c"])))
      for (rule in all_rules) {
        expect_null(check_failures(got, rule), label = rule)
      }
      expect_null(check_failures(got))
    }
  )

  test_that(
    paste0(
      "the error names every failing key, in the order of the rules (",
      kind,
      ")"
    ),
    {
      # a: the live and the staged partitions disagree, so only the live
      # disagreement shows. b: the staged partitions disagree. c: missing.
      # d: a decrease. e: passes. f: not live, and its staged partitions
      # disagree, so it fails.
      live <- rows(
        c("a", "a", "b", "b", "c", "d", "d", "e", "e"),
        c(10, 11, 5, 5, 7, 8, 8, 1, 1),
        c("p1", "p2", "p1", "p2", "p2", "p1", "p2", "p1", "p2")
      )
      staged <- rows(
        c("a", "a", "b", "b", "d", "d", "e", "e", "f", "f"),
        c(12, 13, 6, 7, 2, 2, 1, 1, 1000000, 2),
        c("p1", "p2", "p1", "p2", "p1", "p2", "p1", "p2", "p1", "p2")
      )

      got <- compare_in_run(kind, live, staged)
      err <- tryCatch(check_staged_against_live(got), error = function(e) e)

      expect_s3_class(err, "cs9_staged_check_error")
      expect_identical(
        class(err),
        c("cs9_staged_check_error", "error", "condition")
      )
      problems <- c(
        "the live partitions disagree on the stored total (10, 11)",
        "the partitions disagree on the staged total (6, 7)",
        "staged none, stored 7",
        "staged 2, stored 8",
        "the partitions disagree on the staged total (2, 1000000)"
      )
      expect_identical(
        err$failures,
        data.table::data.table(
          x = c("a", "b", "c", "d", "f"),
          problem = problems
        )
      )
      expect_identical(
        conditionMessage(err),
        paste0(
          "cs9: 5 of 6 keys failed the check of the staged rows against the ",
          "live rows. ",
          paste0(
            "x = ",
            c("a", "b", "c", "d", "f"),
            ": ",
            problems,
            collapse = "; "
          )
        )
      )
    }
  )

  test_that(
    paste0(
      "compare_staged_to_live() outside a task run gives the staged_tbl() error (",
      kind,
      ")"
    ),
    {
      db <- local_staging_db()
      out <- new_compare_out(db, kind)
      seed_cmp_live(out, rows("a", 1, "p1"))

      expected <- tryCatch(out$staged_tbl(), error = conditionMessage)
      expect_match(expected, "works only during a run of a task", fixed = TRUE)
      expect_error(
        compare_staged_to_live(out, "x", "n"),
        expected,
        fixed = TRUE
      )
    }
  )
}

test_that("the error names every key column of a key of two columns", {
  comparison <- data.table::data.table(
    x = c("a", "a", "b"),
    g = c("t", "u", "t"),
    partition = NA_character_,
    staged = c(1, NA, 5),
    live = c(2, 3, 5)
  )

  err <- tryCatch(check_staged_against_live(comparison), error = function(e) e)

  expect_identical(
    err$failures,
    data.table::data.table(
      x = c("a", "a"),
      g = c("t", "u"),
      problem = c("staged 1, stored 2", "staged none, stored 3")
    )
  )
  expect_match(
    conditionMessage(err),
    "x = a, g = t: staged 1, stored 2; x = a, g = u: staged none, stored 3",
    fixed = TRUE
  )
})

test_that("an unknown rule is an error", {
  comparison <- expected_comparison("a", NA_character_, 1, 1)
  expect_error(
    check_staged_against_live(comparison, c("no_missing", "no_increase")),
    "unknown rule no_increase",
    fixed = TRUE
  )
})
