# The generic grid of synthetic tables: the time columns of csfmt_rts_data_v2
# from cstime, and a generator that gives one row per combination of world
# elements.

# The time columns of csfmt_rts_data_v2, in the order that synthetic_weeks()
# returns them.
synthetic_time_names <- c(
  "granularity_time",
  "isoyearweek",
  "isoyear",
  "isoweek",
  "isoquarter",
  "isoyearquarter",
  "season",
  "seasonweek",
  "date",
  "calyear",
  "calmonth",
  "calyearmonth"
)

# The isoyearweeks from `from` to `to`, in order. Stops when either is not an
# isoyearweek of cstime, or when `from` is after `to`.
synthetic_isoyearweeks <- function(from, to) {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek
  for (x in list(from, to)) {
    ok <- is.character(x) && length(x) == 1 && !is.na(x) && x %in% weeks
    if (!ok) {
      stop(
        "from and to must each be one isoyearweek, such as \"2021-30\".",
        call. = FALSE
      )
    }
  }
  if (from > to) {
    stop("from must not be after to.", call. = FALSE)
  }
  return(weeks[weeks >= from & weeks <= to])
}

# The time columns of each isoyearweek, with the granularity_time `gran`.
synthetic_time_of <- function(isoyearweek, gran) {
  date <- cstime::isoyearweek_to_last_date(isoyearweek)
  d <- data.table::data.table(
    granularity_time = gran,
    isoyearweek = isoyearweek,
    isoyear = as.integer(cstime::isoyearweek_to_isoyear_n(isoyearweek)),
    isoweek = as.integer(cstime::isoyearweek_to_isoweek_n(isoyearweek)),
    isoquarter = as.integer(cstime::isoyearweek_to_isoquarter_n(isoyearweek)),
    isoyearquarter = cstime::isoyearweek_to_isoyearquarter_c(isoyearweek),
    season = cstime::isoyearweek_to_season_c(isoyearweek),
    seasonweek = as.numeric(cstime::isoyearweek_to_seasonweek_n(isoyearweek)),
    date = date,
    calyear = as.integer(cstime::date_to_calyear_n(date)),
    calmonth = as.integer(cstime::date_to_calmonth_n(date)),
    calyearmonth = cstime::date_to_calyearmonth_c(date)
  )
  return(d)
}

#' The time columns of weeks and of isoyears
#'
#' @description
#' `synthetic_weeks()` returns one row for each isoyearweek from `from` to
#' `to`. `synthetic_years()` returns one row for each isoyear of those weeks.
#'
#' @details
#' Each function returns the 12 time columns of csfmt_rts_data_v2:
#' `granularity_time`, `isoyearweek`, `isoyear`, `isoweek`, `isoquarter`,
#' `isoyearquarter`, `season`, `seasonweek`, `date`, `calyear`, `calmonth` and
#' `calyearmonth`. cstime derives each column except `granularity_time` from
#' `isoyearweek`. `date` is the last date of the week, and `calyear`,
#' `calmonth` and `calyearmonth` come from `date`. The integer columns are integer, `seasonweek` is
#' numeric, and `date` is a `Date`.
#'
#' `synthetic_weeks()` sets `granularity_time` to `"isoyearweek"`.
#' `synthetic_years()` sets it to `"isoyear"`. A row of `synthetic_years()`
#' takes the last isoyearweek of its isoyear, from
#' `cstime::isoyear_to_last_isoyearweek_c()`. That week can be after `to`.
#'
#' Put the result in the world as a data.frame element. Then
#' [synthetic_grid_generator()] gives each table the time columns that it
#' holds.
#'
#' @param from,to The first and the last isoyearweek, as text, for example
#'   `"2021-30"`.
#' @return A data.table with the 12 time columns.
#' @export
#' @examples
#' synthetic_weeks("2021-30", "2021-32")
#' synthetic_years("2021-30", "2022-10")
synthetic_weeks <- function(from, to) {
  return(synthetic_time_of(synthetic_isoyearweeks(from, to), "isoyearweek"))
}

#' @rdname synthetic_weeks
#' @export
synthetic_years <- function(from, to) {
  weeks <- synthetic_isoyearweeks(from, to)
  isoyear <- sort(unique(cstime::isoyearweek_to_isoyear_n(weeks)))
  last <- cstime::isoyear_to_last_isoyearweek_c(isoyear)
  return(synthetic_time_of(last, "isoyear"))
}

# The unit of one world element in the grid: a vector of unique values, or a
# data.table of the element's columns in `columns`, made unique. NULL when the
# element has no column in `columns`.
synthetic_grid_unit <- function(element, name, columns) {
  if (is.data.frame(element)) {
    keep <- intersect(names(element), columns)
    if (length(keep) == 0) {
      return(NULL)
    }
    d <- data.table::as.data.table(element)[, keep, with = FALSE]
    return(unique(d))
  }
  if (!is.atomic(element)) {
    stop(
      "The world element ",
      name,
      " must be a vector or a data.frame.",
      call. = FALSE
    )
  }
  if (!name %in% columns) {
    return(NULL)
  }
  return(unique(element))
}

# Every row of `a` with every row of `b`. A row of `a` varies slowest.
synthetic_cross_join <- function(a, b) {
  i <- rep(seq_len(nrow(a)), each = nrow(b))
  j <- rep(seq_len(nrow(b)), times = nrow(a))
  return(cbind(a[i], b[j]))
}

# Every combination of the units of the elements `grid` of `world`, for the
# columns `columns`. The vectors are crossed with data.table::CJ() first, and
# each data.frame then with every row so far.
synthetic_grid_rows <- function(world, grid, columns) {
  missing <- setdiff(grid, names(world))
  if (is.null(world) || length(missing) > 0) {
    stop(
      "synthetic_grid_generator() needs the world elements ",
      paste0(missing, collapse = ", "),
      ". Set them with set_synthetic_world().",
      call. = FALSE
    )
  }
  units <- list()
  for (x in grid) {
    units[[x]] <- synthetic_grid_unit(world[[x]], x, columns)
  }
  cols <- unlist(lapply(names(units), function(x) {
    return(if (is.data.frame(units[[x]])) names(units[[x]]) else x)
  }))
  if (length(cols) == 0 || anyDuplicated(cols) > 0) {
    stop(
      "The grid elements must give at least one column of the table, and ",
      "each column at most once. They give: ",
      paste0(cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  is_df <- vapply(units, is.data.frame, logical(1))
  rows <- NULL
  if (any(!is_df)) {
    vectors <- units[!is_df]
    rows <- do.call(data.table::CJ, c(vectors, list(sorted = FALSE)))
  }
  for (u in units[is_df]) {
    rows <- if (is.null(rows)) u else synthetic_cross_join(rows, u)
  }
  return(rows)
}

#' A generator that gives one row per combination of world elements
#'
#' @description
#' Returns a `function(profile, n, seed, world)` for the `generator` of a
#' synthetic spec. The function gives one row for each combination of the
#' world elements in `grid`, and draws the other columns with
#' `csdb::simulate_from_profile()`.
#'
#' @details
#' The generator works in this order:
#'
#' 1. It stops when `world` is `NULL` or does not hold every element of
#'    `grid`.
#' 2. It takes one unit from each element of `grid`. A vector is one column
#'    of unique values. A data.frame gives its columns that are in
#'    `names(profile$field_types)`, as unique rows, so the values in one row
#'    stay together. An element with no column in the table is skipped.
#' 3. It crosses the units. `data.table::CJ()` crosses the vectors, in the
#'    order of `grid`, and each data.frame is then crossed with every row so
#'    far. The grid is the result.
#' 4. It calls `csdb::simulate_from_profile()` for `nrow(grid)` rows, with
#'    `seed`. It removes `profile$keys`, because the grid makes the rows
#'    unique. It removes every data.frame element from the world. csdb
#'    draws wrong values for a data.frame element named like a TEXT column,
#'    and stops for one named like an INTEGER column.
#' 5. It replaces each grid column of the draw with the grid.
#' 6. It calls `finish(d, seed, world)` when `finish` is not `NULL`. `world`
#'    is the world as the generator got it, with its data.frame elements.
#' 7. It orders the columns as `names(profile$field_types)`, and returns the
#'    data.table.
#'
#' The generator ignores `n`: the grid sets the number of rows. For a
#' partitioned table, put the partition column in the world and in `grid`,
#' so that every partition gets every combination.
#'
#' The generator stops when the elements of `grid` give no column of the
#' table, or give one column twice. It installs the grid values with their R
#' types, so a world element MUST have the R type of its column.
#' [synthetic_weeks()] and [synthetic_years()] give the time columns with
#' those types.
#'
#' @param grid A character vector of names of world elements.
#' @param finish `NULL`, or a `function(d, seed, world)` that returns the
#'   data.table `d` with values for one table.
#' @return A `function(profile, n, seed, world)` that returns a data.table.
#' @export
#' @examples
#' generator <- synthetic_grid_generator(c("location", "age", "weeks"))
#' world <- list(
#'   location = data.frame(
#'     location_code = c("nation_nor", "county_nor03"),
#'     granularity_geo = c("nation", "county")
#'   ),
#'   age = c("total", "000_004"),
#'   weeks = synthetic_weeks("2024-01", "2024-03")
#' )
#' profile <- csdb::profile_schema(
#'   c(location_code = "TEXT", granularity_geo = "TEXT", age = "TEXT",
#'     isoyearweek = "TEXT", n = "INTEGER"),
#'   keys = c("location_code", "age", "isoyearweek")
#' )
#' generator(profile, n = NULL, seed = 1L, world = world)
synthetic_grid_generator <- function(grid, finish = NULL) {
  ok <- is.character(grid) && length(grid) > 0 && !anyNA(grid)
  if (!ok) {
    stop(
      "grid must be a character vector of world element names.",
      call. = FALSE
    )
  }
  if (!is.null(finish) && !is.function(finish)) {
    stop("finish must be NULL or a function(d, seed, world).", call. = FALSE)
  }
  force(finish)
  return(function(profile, n, seed, world) {
    columns <- names(profile$field_types)
    rows <- synthetic_grid_rows(world, grid, columns)
    profile$keys <- NULL
    is_df <- vapply(world, is.data.frame, logical(1))
    d <- csdb::simulate_from_profile(
      profile,
      n = nrow(rows),
      seed = seed,
      world = world[!is_df]
    )
    for (x in names(rows)) {
      data.table::set(d, j = x, value = rows[[x]])
    }
    if (!is.null(finish)) {
      d <- finish(d, seed, world)
    }
    data.table::setcolorder(d, intersect(columns, names(d)))
    return(d[])
  })
}
