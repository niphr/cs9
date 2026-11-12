# synthetic_grid_generator(), synthetic_weeks(), synthetic_years() and
# populate_synthetic(derive = ).
#
# local_staging_db() is in helper-run-staging.R. synth_error(),
# local_synth_system() and synth_seed_of() are copies of the helpers of the
# same names in test-synthetic.R, because each test file has its own
# environment.

# The message of the error that `expr` signals, or "" when it succeeds. A
# call that should stop and does not then fails an expectation.
synth_error <- function(expr) {
  return(tryCatch(
    {
      force(expr)
      ""
    },
    error = function(e) conditionMessage(e)
  ))
}

# A system whose tables are disconnected when the test ends.
local_synth_system <- function(envir = parent.frame()) {
  ss <- SurveillanceSystem_v9$new(name = "synthtest")
  withr::defer(
    for (t in c(ss$tables, ss$partitionedtables)) {
      t$disconnect()
    },
    envir = envir
  )
  return(ss)
}

# The seed that populate_synthetic() gives a table, from the documented rule:
# seed plus the first 7 hexadecimal digits of the md5 of the table name,
# modulo .Machine$integer.max.
synth_seed_of <- function(seed, table_name) {
  f <- withr::local_tempfile()
  writeBin(charToRaw(table_name), f)
  offset <- strtoi(substr(unname(tools::md5sum(f)), 1, 7), 16L)
  return(as.integer((seed + offset) %% .Machine$integer.max))
}

grid_time_fields <- c(
  granularity_time = "TEXT",
  isoyearweek = "TEXT",
  isoyear = "INTEGER",
  isoweek = "INTEGER",
  isoquarter = "INTEGER",
  isoyearquarter = "TEXT",
  season = "TEXT",
  seasonweek = "DOUBLE",
  date = "DATE",
  calyear = "INTEGER",
  calmonth = "INTEGER",
  calyearmonth = "TEXT"
)
grid_fields <- c(
  granularity_geo = "TEXT",
  location_code = "TEXT",
  age = "TEXT",
  sex = "TEXT",
  grid_time_fields,
  n = "INTEGER",
  rate = "DOUBLE"
)
grid_keys <- c("location_code", "age", "sex", "isoyearweek")

# Each location_code has one granularity_geo. A grid that crosses the two
# columns as independent vectors gives pairs that are not in this table.
grid_locations <- data.frame(
  location_code = c("nation_nor", "county_nor03", "county_nor11"),
  granularity_geo = c("nation", "county", "county")
)

grid_world <- function(
  from = "2024-01",
  to = "2024-10",
  age = c("total", "000_004")
) {
  return(list(
    location = grid_locations,
    age = age,
    sex = c("total", "female", "male"),
    weeks = synthetic_weeks(from, to),
    part = c("p1", "p2")
  ))
}

grid_elements <- c("location", "age", "sex", "weeks", "part")

# The partitioned table anon_grid_counts and the plain table anon_grid_rates,
# both with the grid generator over grid_elements. The rates table has no
# column part, so its grid skips that element.
add_grid_tables <- function(ss, finish = NULL) {
  generator <- synthetic_grid_generator(grid_elements, finish = finish)
  ss$add_partitionedtable(
    name_access = "anon",
    name_grouping = "grid",
    name_variant = "counts",
    name_partitions = c("p1", "p2"),
    column_name_partition = "part",
    field_types = c(grid_fields, part = "TEXT"),
    keys = grid_keys,
    synthetic = list(generator = generator)
  )
  ss$add_table(
    name_access = "anon",
    name_grouping = "grid",
    name_variant = "rates",
    field_types = grid_fields,
    keys = grid_keys,
    synthetic = list(generator = generator)
  )
  return(list(
    counts = ss$partitionedtables$anon_grid_counts,
    rates = ss$tables$anon_grid_rates
  ))
}

grid_targets <- c("anon_grid_counts", "anon_grid_rates")

# Data with the columns in name order and the rows ordered by every key
# column.
grid_order <- function(d) {
  d <- data.table::copy(data.table::as.data.table(d))
  data.table::setcolorder(d, sort(names(d)))
  if (nrow(d) > 0) {
    data.table::setorderv(d, intersect(c(grid_keys, "part"), names(d)))
  }
  return(d)
}

# Every row of a table without auto_last_updated_datetime, in grid_order().
grid_rows <- function(table) {
  children <- if (inherits(table, "DBPartitionedTableExtended_v9")) {
    table$tables
  } else {
    list(table)
  }
  d <- data.table::rbindlist(lapply(children, function(t) {
    return(data.table::as.data.table(dplyr::collect(t$tbl())))
  }))
  if ("auto_last_updated_datetime" %in% names(d)) {
    d[, auto_last_updated_datetime := NULL]
  }
  return(grid_order(d))
}

# The row count and every row of each table, to show that a call wrote
# nothing.
grid_snapshot <- function(tables) {
  return(lapply(tables, function(t) {
    d <- grid_rows(t)
    return(list(n = nrow(d), rows = d))
  }))
}

test_that("grid: one row per combination with unique keys at 65,700 rows", {
  local_staging_db()
  ss <- local_synth_system()
  add_grid_tables(ss)
  ages <- c("total", "000_004", "005_014", "015_064", "065p")
  world <- grid_world("2010-01", "2023-52", age = ages)
  ss$set_synthetic_world(world)

  d <- ss$synthetic("anon_grid_counts", n = NULL, seed = 1L)
  rates <- ss$synthetic("anon_grid_rates", n = NULL, seed = 1L)

  n_weeks <- nrow(world$weeks)
  expect_equal(n_weeks, 730L)
  expect_equal(nrow(d), 3L * 5L * 3L * n_weeks * 2L)
  expect_gte(nrow(d), 50000L)
  expect_equal(anyDuplicated(d, by = c(grid_keys, "part")), 0L)
  expect_equal(names(d), names(c(grid_fields, part = "TEXT")))
  # The plain table has no column part: its grid skips that element.
  expect_equal(nrow(rates), 3L * 5L * 3L * n_weeks)
  expect_equal(anyDuplicated(rates, by = grid_keys), 0L)
  # Each grid column takes every value of its world element.
  expect_setequal(d$age, ages)
  expect_setequal(d$part, c("p1", "p2"))
  expect_setequal(d$isoyearweek, world$weeks$isoyearweek)
  # The time columns of each row are those of its week.
  time <- unique(d[, names(grid_time_fields), with = FALSE])
  data.table::setorderv(time, "isoyearweek")
  expect_equal(time, world$weeks, ignore_attr = TRUE)
})

test_that("(a) grid: a data.frame world element keeps its rows together", {
  # The generator alone, without the validation of a SyntheticTable, so that
  # the pairs are checked here and not by the key check of csdb.
  generator <- synthetic_grid_generator(grid_elements)
  profile <- csdb::profile_schema(
    c(grid_fields, part = "TEXT"),
    keys = c(grid_keys, "part")
  )

  d <- generator(profile, n = NULL, seed = 1L, world = grid_world())

  pairs <- unique(d[, c("location_code", "granularity_geo")])
  data.table::setorderv(pairs, "location_code")
  want <- data.table::as.data.table(grid_locations)
  data.table::setorderv(want, "location_code")
  expect_equal(pairs, want, ignore_attr = TRUE)
  geo <- grid_locations$granularity_geo[
    match(d$location_code, grid_locations$location_code)
  ]
  expect_identical(d$granularity_geo, geo)
  expect_equal(nrow(d), 3L * 2L * 3L * 10L * 2L)
})

test_that("grid: the generator stops without the world elements of its grid", {
  generator <- synthetic_grid_generator(c("location", "age"))
  profile <- csdb::profile_schema(grid_fields, keys = grid_keys)

  expect_error(
    generator(profile, n = NULL, seed = 1L, world = NULL),
    "needs the world elements location, age"
  )
  expect_error(
    generator(profile, n = NULL, seed = 1L, world = list(age = "total")),
    "needs the world elements location\\. "
  )
  expect_error(synthetic_grid_generator(1:2), "character vector")
})

test_that("grid: finish gets the data with the grid columns set", {
  local_staging_db()
  ss <- local_synth_system()
  seen <- new.env()
  finish <- function(d, seed, world) {
    seen$d <- data.table::copy(d)
    seen$seed <- seed
    seen$world <- world
    d[, n := 7L]
    return(d)
  }
  add_grid_tables(ss, finish = finish)
  world <- grid_world()
  ss$set_synthetic_world(world)

  d <- ss$synthetic("anon_grid_counts", n = NULL, seed = 4L)

  expect_identical(seen$seed, 4L)
  expect_identical(seen$world, world)
  expect_equal(nrow(seen$d), nrow(d))
  expect_identical(seen$d$location_code, d$location_code)
  expect_identical(seen$d$granularity_geo, d$granularity_geo)
  expect_identical(seen$d$isoyearweek, d$isoyearweek)
  expect_identical(seen$d$part, d$part)
  expect_setequal(seen$d$location_code, grid_locations$location_code)
  expect_true(all(d$n == 7L))
})

test_that("(e) synthetic_weeks() and synthetic_years() give every time column from cstime", {
  check_time <- function(d) {
    w <- d$isoyearweek
    date <- cstime::isoyearweek_to_last_date(w)
    expect_identical(names(d), names(grid_time_fields))
    expect_identical(
      d$isoyear,
      as.integer(cstime::isoyearweek_to_isoyear_n(w))
    )
    expect_identical(
      d$isoweek,
      as.integer(cstime::isoyearweek_to_isoweek_n(w))
    )
    expect_identical(
      d$isoquarter,
      as.integer(cstime::isoyearweek_to_isoquarter_n(w))
    )
    expect_identical(
      d$isoyearquarter,
      cstime::isoyearweek_to_isoyearquarter_c(w)
    )
    expect_identical(d$season, cstime::isoyearweek_to_season_c(w))
    expect_identical(
      d$seasonweek,
      as.numeric(cstime::isoyearweek_to_seasonweek_n(w))
    )
    expect_identical(d$date, date)
    expect_s3_class(d$date, "Date")
    expect_identical(d$calyear, as.integer(cstime::date_to_calyear_n(date)))
    expect_identical(d$calmonth, as.integer(cstime::date_to_calmonth_n(date)))
    expect_identical(d$calyearmonth, cstime::date_to_calyearmonth_c(date))
    return(invisible(NULL))
  }
  all_weeks <- cstime::dates_by_isoyearweek$isoyearweek
  weeks <- synthetic_weeks("2020-50", "2022-03")
  years <- synthetic_years("2020-50", "2022-03")

  check_time(weeks)
  check_time(years)
  expect_identical(
    weeks$isoyearweek,
    all_weeks[all_weeks >= "2020-50" & all_weeks <= "2022-03"]
  )
  expect_true(all(weeks$granularity_time == "isoyearweek"))
  # 2020 has 53 isoweeks. A yearly row takes the last week of its isoyear.
  expect_identical(years$isoyear, 2020:2022)
  expect_identical(years$isoyearweek, c("2020-53", "2021-52", "2022-52"))
  expect_identical(
    years$isoyearweek,
    cstime::isoyear_to_last_isoyearweek_c(2020:2022)
  )
  expect_true(all(years$granularity_time == "isoyear"))
  expect_error(synthetic_weeks("2022-03", "2020-50"), "after")
  expect_error(synthetic_weeks("2022-99", "2023-01"), "isoyearweek")
})

test_that("(g) grid: a data.frame element named like a column gives correct values", {
  local_staging_db()
  ss <- local_synth_system()
  ss$add_table(
    name_access = "anon",
    name_grouping = "grid",
    name_variant = "yearly",
    field_types = grid_fields,
    keys = c("location_code", "isoyear"),
    synthetic = list(
      generator = synthetic_grid_generator(c("location_code", "isoyear"))
    )
  )
  # Both elements are data.frames named like one of their columns. isoyear is
  # an INTEGER column and location_code a TEXT column.
  years <- synthetic_years("2020-01", "2023-52")
  ss$set_synthetic_world(list(location_code = grid_locations, isoyear = years))

  # An empty table, so that a failed draw fails the expectations below
  # instead of stopping the test with an error.
  d <- data.table::data.table(
    location_code = character(0),
    granularity_geo = character(0),
    isoyear = integer(0),
    isoyearweek = character(0),
    granularity_time = character(0)
  )

  expect_no_error(d <- ss$synthetic("anon_grid_yearly", n = NULL, seed = 1L))
  expect_setequal(d$location_code, grid_locations$location_code)
  expect_identical(
    d$granularity_geo,
    grid_locations$granularity_geo[
      match(d$location_code, grid_locations$location_code)
    ]
  )
  expect_setequal(d$isoyear, 2020:2023)
  expect_identical(
    d$isoyearweek,
    years$isoyearweek[match(d$isoyear, years$isoyear)]
  )
  expect_true(all(d$granularity_time == "isoyear"))
})

test_that("(h) grid: a grid that fills the key space draws unique keys", {
  local_staging_db()
  ss <- local_synth_system()
  # Every key column is a TEXT vector of the world, so the key space of a
  # random draw is the grid itself: 3 x 3 x 120 x 2 = 2,160 rows.
  ss$add_partitionedtable(
    name_access = "anon",
    name_grouping = "grid",
    name_variant = "full",
    name_partitions = c("p1", "p2"),
    column_name_partition = "part",
    field_types = c(
      age = "TEXT",
      sex = "TEXT",
      isoyearweek = "TEXT",
      n = "INTEGER",
      part = "TEXT"
    ),
    keys = c("age", "sex", "isoyearweek"),
    synthetic = list(
      generator = synthetic_grid_generator(
        c("age", "sex", "isoyearweek", "part")
      )
    )
  )
  weeks <- synthetic_weeks("2020-01", "2022-15")$isoyearweek
  ss$set_synthetic_world(list(
    age = c("total", "000_004", "005_014"),
    sex = c("total", "female", "male"),
    isoyearweek = weeks,
    part = c("p1", "p2")
  ))

  err <- synth_error(d <- ss$synthetic("anon_grid_full", n = NULL, seed = 1L))

  expect_equal(length(weeks), 120L)
  expect_equal(err, "")
  expect_equal(nrow(d), 2160L)
  expect_equal(
    anyDuplicated(d, by = c("age", "sex", "isoyearweek", "part")),
    0L
  )
})

test_that("(b) derive sees the tables that populate_synthetic() drew", {
  local_staging_db()
  ss <- local_synth_system()
  tabs <- add_grid_tables(ss)
  ss$set_synthetic_world(grid_world())
  ss$set_synthetic(TRUE)
  seen <- new.env()
  derive_rates <- function(data, world, seed) {
    seen$names <- names(data)
    seen$counts <- data.table::copy(data$anon_grid_counts)
    seen$world <- world
    seen$seed <- seed
    totals <- data$anon_grid_counts[, .(total = sum(n)), by = grid_keys]
    d <- data.table::copy(data$anon_grid_rates)
    d[totals, on = grid_keys, rate := as.numeric(i.total) / 1000]
    seen$rates <- data.table::copy(d)
    return(d)
  }

  # NULL until the call returns, so that a failed call fails the
  # expectations below instead of stopping the test with an error.
  written <- NULL
  err <- synth_error(written <- ss$populate_synthetic(
    tables = grid_targets,
    n = NULL,
    seed = 5L,
    derive = list(anon_grid_rates = derive_rates)
  ))

  # The function gets the data of every table, drawn before it runs.
  expect_identical(seen$names, grid_targets)
  expect_equal(err, "")
  expect_equal(written, c(anon_grid_counts = 360L, anon_grid_rates = 180L))
  # Every column and every row of the drawn counts is what the table holds
  # after the write. Both sides are in grid_order(): columns by name, rows by
  # the keys. The SQLite round trip keeps every R type, the Date column
  # included, so the read-back side needs no conversion.
  counts <- grid_rows(tabs$counts)
  expect_equal(grid_order(seen$counts), counts)
  # The rates that the function returned are the rates that were written.
  expect_equal(grid_order(seen$rates), grid_rows(tabs$rates))
  expect_identical(seen$world, ss$synthetic_world)
  expect_identical(seen$seed, synth_seed_of(5L, "anon_grid_rates"))
  rates <- grid_rows(tabs$rates)
  totals <- counts[, .(total = sum(n)), by = grid_keys]
  both <- merge(rates, totals, by = grid_keys)
  expect_equal(nrow(both), 180L)
  expect_equal(both$rate, both$total / 1000)
})

test_that("(c) a derive that breaks its own table stops populate and writes nothing", {
  local_staging_db()
  ss <- local_synth_system()
  tabs <- add_grid_tables(ss)
  ss$set_synthetic_world(grid_world())
  ss$set_synthetic(TRUE)
  ss$populate_synthetic(tables = grid_targets, n = NULL, seed = 1L)
  before <- grid_snapshot(tabs)
  bad_rates <- function(data, world, seed) {
    d <- data.table::copy(data$anon_grid_rates)
    d[, rate := as.character(rate)]
    return(d)
  }

  err <- synth_error(ss$populate_synthetic(
    tables = grid_targets,
    n = NULL,
    seed = 2L,
    derive = list(anon_grid_rates = bad_rates)
  ))

  expect_match(err, "anon_grid_rates.*wrong R type: rate \\(DOUBLE\\)")
  expect_equal(before$counts$n, 360L)
  expect_equal(before$rates$n, 180L)
  expect_equal(grid_snapshot(tabs), before)
})

test_that("(d) a derive name that is not a target stops before anything is written", {
  local_staging_db()
  ss <- local_synth_system()
  tabs <- add_grid_tables(ss)
  ss$set_synthetic_world(grid_world())
  ss$set_synthetic(TRUE)
  ss$populate_synthetic(tables = grid_targets, n = NULL, seed = 1L)
  before <- grid_snapshot(tabs)
  called <- new.env()
  called$n <- 0L
  f <- function(data, world, seed) {
    called$n <- called$n + 1L
    return(data$anon_grid_rates)
  }

  err <- synth_error(ss$populate_synthetic(
    tables = grid_targets,
    n = NULL,
    seed = 2L,
    derive = list(anon_grid_other = f)
  ))

  expect_match(
    err,
    "derive names tables that populate_synthetic\\(\\) does not fill: anon_grid_other"
  )
  expect_equal(called$n, 0L)
  expect_equal(before$counts$n, 360L)
  expect_equal(grid_snapshot(tabs), before)
  expect_match(
    synth_error(ss$populate_synthetic(
      tables = grid_targets,
      n = NULL,
      derive = list(f)
    )),
    "named by a different table"
  )
  expect_equal(grid_snapshot(tabs), before)
})

test_that("(f) a derive that breaks another table stops populate and writes nothing", {
  local_staging_db()
  ss <- local_synth_system()
  tabs <- add_grid_tables(ss)
  ss$set_synthetic_world(grid_world())
  ss$set_synthetic(TRUE)
  ss$populate_synthetic(tables = grid_targets, n = NULL, seed = 1L)
  before <- grid_snapshot(tabs)
  # The derive of the rates changes the column n of the counts by reference,
  # and returns valid rates.
  break_counts <- function(data, world, seed) {
    counts <- data$anon_grid_counts
    data.table::set(counts, j = "n", value = as.character(counts$n))
    return(data$anon_grid_rates)
  }

  err <- synth_error(ss$populate_synthetic(
    tables = grid_targets,
    n = NULL,
    seed = 2L,
    derive = list(anon_grid_rates = break_counts)
  ))

  expect_match(err, "anon_grid_counts.*wrong R type: n \\(INTEGER\\)")
  expect_equal(before$counts$n, 360L)
  expect_equal(grid_snapshot(tabs), before)
})

test_that("derive runs in list order, each function on the data so far", {
  local_staging_db()
  ss <- local_synth_system()
  tabs <- add_grid_tables(ss)
  ss$set_synthetic_world(grid_world())
  ss$set_synthetic(TRUE)
  set_counts <- function(data, world, seed) {
    d <- data.table::copy(data$anon_grid_counts)
    d[, n := 3L]
    return(d)
  }
  rates_from_counts <- function(data, world, seed) {
    totals <- data$anon_grid_counts[, .(total = sum(n)), by = grid_keys]
    d <- data.table::copy(data$anon_grid_rates)
    d[totals, on = grid_keys, rate := as.numeric(i.total)]
    return(d)
  }

  ss$populate_synthetic(
    tables = grid_targets,
    n = NULL,
    derive = list(
      anon_grid_counts = set_counts,
      anon_grid_rates = rates_from_counts
    )
  )

  expect_true(all(grid_rows(tabs$counts)$n == 3L))
  # Two partitions of 3 each.
  expect_true(all(grid_rows(tabs$rates)$rate == 6))
})

test_that("populate_synthetic() with derive = NULL or list() writes the same rows", {
  local_staging_db()
  ss <- local_synth_system()
  tabs <- add_grid_tables(ss)
  ss$set_synthetic_world(grid_world())
  ss$set_synthetic(TRUE)

  ss$populate_synthetic(tables = grid_targets, n = NULL, seed = 3L)
  first <- grid_snapshot(tabs)
  ss$populate_synthetic(
    tables = grid_targets,
    n = NULL,
    seed = 3L,
    derive = NULL
  )
  second <- grid_snapshot(tabs)
  ss$populate_synthetic(
    tables = grid_targets,
    n = NULL,
    seed = 3L,
    derive = list()
  )
  third <- grid_snapshot(tabs)

  expect_equal(second, first)
  expect_equal(third, first)
})
