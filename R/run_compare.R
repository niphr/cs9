# The two functions below compare the rows that a task run staged in a table
# with the live rows, and check the comparison against rules. They follow
# consultations_check() in norsyss.cs9, which a task called before the publish.

#' Compare the staged rows of a task run with the live rows
#'
#' @description
#' `compare_staged_to_live()` reads the rows that the task run in progress
#' staged in `table`, and the live rows of `table`. It returns the values of
#' one column of both, side by side, for each key and partition.
#'
#' @details
#' The function reads the staged rows with `table$staged_tbl()`. It reads the
#' live rows with `tbl()` of `table`, or of each partition table of a
#' partitioned table. It skips a live table that does not exist. It collects
#' each side with [csdb::collect_and_sort_by_pk()].
#'
#' The function works only during a task run of a task that has `table` in
#' its `tables`. Outside a run it stops with the error of `staged_tbl()`.
#'
#' A row of the result holds at most one staged value and one live value.
#' Several rows of one side can share a key in one partition, for example
#' when `key` is not the whole primary key. The function then keeps each
#' distinct value. The result then has one row for each combination of a
#' distinct staged value and a distinct live value of that key and partition.
#' [check_staged_against_live()] then sees the disagreement.
#'
#' The function reads a value of `NA` as no value.
#'
#' @param table A `DBTableExtended_v9` or a `DBPartitionedTableExtended_v9`
#'   of the task run, as `tables$<name>` in the action of the task.
#' @param key A character vector of column names. The values of these
#'   columns identify a key. The vector MUST NOT contain `partition`,
#'   `staged`, `live` or `problem`. The result and
#'   [check_staged_against_live()] use these names.
#' @param value The name of one column. Its values MUST be numeric.
#' @param filter `NULL`, or a function that takes a lazy table and returns a
#'   lazy table. The function applies it to the staged rows and to each live
#'   table before it collects them.
#' @return A data.table with the `key` columns and these columns:
#'
#'   - `partition`: character. The name of the partition, as in
#'     `names(table$tables)`. `NA` for a table without partitions.
#'   - `staged`: numeric. The staged value, or `NA` where no staged row holds
#'     the key in that partition.
#'   - `live`: numeric. The live value, or `NA` where no live row holds the key
#'     in that partition.
#'
#'   The rows are sorted by the `key` columns, `partition`, `staged` and
#'   `live`, with `NA` first.
#' @examples
#' \dontrun{
#' # In the action of a task that has the table `results` in its tables.
#' weeks <- c("2026-45", "2026-46")
#' comparison <- compare_staged_to_live(
#'   tables$results,
#'   key = "isoyearweek",
#'   value = "n",
#'   filter = function(x) dplyr::filter(x, isoyearweek %in% !!weeks)
#' )
#' check_staged_against_live(comparison)
#' }
#' @seealso [check_staged_against_live()]
#' @export
compare_staged_to_live <- function(table, key, value, filter = NULL) {
  staged <- table$staged_tbl()
  reserved <- intersect(key, c("partition", "staged", "live", "problem"))
  if (length(reserved) > 0) {
    stop(
      "cs9: `key` MUST NOT contain ",
      paste(reserved, collapse = ", "),
      ", because the comparison uses these names.",
      call. = FALSE
    )
  }
  partitioned <- inherits(table, "DBPartitionedTableExtended_v9")
  lives <- if (partitioned) table$tables else list(table)
  part_col <- if (partitioned) table$column_name_partition else NULL

  read <- function(x, columns) {
    if (!is.null(filter)) {
      x <- filter(x)
    }
    x <- dplyr::select(x, dplyr::all_of(columns))
    return(csdb::collect_and_sort_by_pk(x))
  }
  # One row per distinct value of a key and partition, with the value in
  # `side`.
  shape <- function(d, partition, side) {
    out <- d[, key, with = FALSE]
    data.table::set(out, j = "partition", value = partition)
    data.table::set(out, j = side, value = as.numeric(d[[value]]))
    return(unique(out[!is.na(out[[side]])]))
  }

  d <- read(staged, c(key, part_col, value))
  staged_partition <- if (partitioned) {
    as.character(d[[part_col]])
  } else {
    rep(NA_character_, nrow(d))
  }
  staged <- shape(d, staged_partition, "staged")

  live <- list()
  for (i in seq_along(lives)) {
    tb <- lives[[i]]
    if (!tb$table_exists()) {
      next
    }
    d <- read(tb$tbl(), c(key, value))
    name <- if (partitioned) names(lives)[i] else NA_character_
    live[[length(live) + 1L]] <- shape(d, rep(name, nrow(d)), "live")
  }
  live <- data.table::rbindlist(live, use.names = TRUE)

  # An empty side takes the column types of the other side, so the merge
  # joins columns of one type.
  if (nrow(staged) == 0 && nrow(live) > 0) {
    staged <- data.table::setnames(live[0], "live", "staged")
  } else if (nrow(live) == 0) {
    live <- data.table::setnames(data.table::copy(staged[0]), "staged", "live")
  }
  by <- c(key, "partition")
  out <- merge(
    staged,
    live,
    by = by,
    all = TRUE,
    allow.cartesian = TRUE
  )
  data.table::setcolorder(out, c(by, "staged", "live"))
  # merge() sets a data.table key. The result carries none.
  data.table::setkey(out, NULL)
  return(csdb::setorder_by_pk(out, list(keys = by)))
}

#' Check a comparison of staged and live rows against rules
#'
#' @description
#' `check_staged_against_live()` checks each key of a comparison from
#' [compare_staged_to_live()] against the rules in `rules`. It stops with an
#' error that names every failing key, and otherwise returns the comparison.
#'
#' @details
#' A key is live when a live row of any partition holds it. These are the
#' rules:
#'
#' - `partitions_agree`: the partitions that hold the key have one value in
#'   the live rows, and one value in the staged rows. "One value" means that
#'   [unique()] of the values has length 1.
#' - `no_missing`: a live key has a staged value.
#' - `no_decrease`: no staged value of a live key is below a live value of
#'   it. A staged value equal to the live value passes.
#'
#' `no_missing` and `no_decrease` pass a key that is not live.
#' `partitions_agree` checks every key, also a key that is not live.
#'
#' A key fails at most once, with the first problem in this order:
#'
#' 1. `the live partitions disagree on the stored total (<values>)`
#' 2. `the partitions disagree on the staged total (<values>)`
#' 3. `staged none, stored <values>`
#' 4. `staged <values>, stored <values>`
#'
#' `<values>` are the distinct values, sorted, without scientific notation,
#' and separated by ", ". These texts and this order are those of
#' `consultations_check()` in norsyss.cs9.
#'
#' @param comparison A data.table from [compare_staged_to_live()]. Every
#'   column except `partition`, `staged` and `live` is a key column.
#' @param rules A character vector with one or more of `"partitions_agree"`,
#'   `"no_missing"` and `"no_decrease"`. An unknown name is an error.
#' @return `comparison`, invisibly, when every key passes. Otherwise the
#'   function stops with an error of class `cs9_staged_check_error`. Its
#'   message names each failing key and its problem. Its element `failures`
#'   is a data.table with the key columns and `problem`, with one row per
#'   failing key.
#' @examples
#' \dontrun{
#' # In the action of a task that has the table `results` in its tables.
#' comparison <- compare_staged_to_live(tables$results, "isoyearweek", "n")
#' check_staged_against_live(comparison, rules = "no_decrease")
#' }
#' @seealso [compare_staged_to_live()]
#' @export
check_staged_against_live <- function(
  comparison,
  rules = c("partitions_agree", "no_missing", "no_decrease")
) {
  known <- c("partitions_agree", "no_missing", "no_decrease")
  unknown <- setdiff(rules, known)
  if (length(unknown) > 0) {
    stop(
      "cs9: unknown rule ",
      paste(unknown, collapse = ", "),
      ". The rules are ",
      paste(known, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  staged <- live <- problem <- NULL
  d <- data.table::as.data.table(comparison)
  key <- setdiff(names(d), c("partition", "staged", "live"))
  # Fixed names for the key columns, so that no key column can mask `rules`
  # or `compare_problem` in the grouped call below.
  inner <- paste0("cs9_key_", seq_along(key))
  d <- data.table::setnames(
    d[, c(key, "staged", "live"), with = FALSE],
    key,
    inner
  )
  failures <- d[,
    list(problem = compare_problem(staged, live, rules)),
    by = inner
  ][!is.na(problem)]
  if (nrow(failures) == 0) {
    return(invisible(comparison))
  }
  data.table::setnames(failures, inner, key)
  data.table::setattr(failures, "sorted", NULL)
  named <- do.call(
    paste,
    c(
      lapply(key, function(k) paste0(k, " = ", failures[[k]])),
      sep = ", "
    )
  )
  n_keys <- nrow(unique(d[, inner, with = FALSE]))
  message <- paste0(
    "cs9: ",
    nrow(failures),
    " of ",
    n_keys,
    " keys failed the check of the staged rows against the live rows. ",
    paste0(named, ": ", failures$problem, collapse = "; ")
  )
  stop(structure(
    class = c("cs9_staged_check_error", "error", "condition"),
    list(message = message, call = NULL, failures = failures)
  ))
}

# The problem of one key, or NA when it passes. `staged` and `live` are the
# values of the rows of the key. See check_staged_against_live().
compare_problem <- function(staged, live, rules) {
  st <- unique(live[!is.na(live)])
  sg <- unique(staged[!is.na(staged)])
  # In the order of the rules. A key that is not live passes no_missing and
  # no_decrease.
  problems <- c(
    if ("partitions_agree" %in% rules) compare_disagreement(sg, st),
    if (length(st) > 0) compare_live_problem(sg, st, rules)
  )
  problems <- problems[!is.na(problems)]
  return(if (length(problems) > 0) problems[[1]] else NA_character_)
}

# The problem of rule no_missing or no_decrease for a live key, or NA. `sg`
# and `st` are the distinct staged and live values of the key.
compare_live_problem <- function(sg, st, rules) {
  if ("no_missing" %in% rules && length(sg) == 0) {
    return(paste0("staged none, stored ", compare_shown(st)))
  }
  if ("no_decrease" %in% rules && length(sg) > 0 && min(sg) < max(st)) {
    return(paste0(
      "staged ",
      compare_shown(sg),
      ", stored ",
      compare_shown(st)
    ))
  }
  return(NA_character_)
}

# The problem of rule partitions_agree, or NA. `sg` and `st` are the distinct
# staged and live values of one key. The live disagreement comes first.
compare_disagreement <- function(sg, st) {
  if (length(st) > 1) {
    return(paste0(
      "the live partitions disagree on the stored total (",
      compare_shown(st),
      ")"
    ))
  }
  if (length(sg) > 1) {
    return(paste0(
      "the partitions disagree on the staged total (",
      compare_shown(sg),
      ")"
    ))
  }
  return(NA_character_)
}

# The values in a problem text: sorted, without scientific notation, and
# separated by ", ".
compare_shown <- function(x) {
  return(paste(
    format(sort(x), scientific = FALSE, trim = TRUE),
    collapse = ", "
  ))
}
