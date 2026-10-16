# cs9 writes its own config tables, the objects in config$tables, with one SQL
# statement over the connection of the table. Until csdb 2026.10.17 each csdb
# load on PostgreSQL started a client, and each client was one more login. A
# forked worker writes these tables in every plan, and production logs in
# through a token proxy that fails under many logins. The csdb JSON load now
# uses the open connection too, so it makes no extra login either. Other
# drivers keep the csdb load.
#
# An upsert is INSERT ... ON CONFLICT on the keys of the table. It needs the
# primary key that csdb creates with the table. It updates every other column,
# as the csdb upsert does. An insert has no ON CONFLICT, so a duplicate key
# stops it, as it stops the csdb load.
config_write <- function(table, rows, upsert = TRUE) {
  if (!run_driver_supported(table$dbconnection$config$driver)) {
    if (upsert) {
      return(table$upsert_data(rows))
    }
    return(table$insert_data(rows))
  }
  if (is.null(rows) || nrow(rows) == 0L) {
    return(invisible(NULL))
  }
  # connect() creates the table at its first use, as the csdb load does.
  table$connect()
  con <- table$dbconnection$connection
  fields <- names(table$field_types)
  rows <- rows[, fields, with = FALSE]
  sqlite <- identical(toupper(table$dbconnection$config$driver), "SQLITE")
  if (sqlite) {
    # SQLite binds each value as the csdb load binds it, so a Date is stored
    # as a number in both. One statement for all rows is one transaction.
    one_row <- paste0(
      "(",
      paste(rep("?", length(fields)), collapse = ", "),
      ")"
    )
    values <- paste(rep(one_row, nrow(rows)), collapse = ", ")
    params <- unlist(
      lapply(seq_len(nrow(rows)), function(i) {
        return(lapply(rows, function(v) v[i]))
      }),
      recursive = FALSE
    )
  } else {
    values <- config_sql_values(con, rows)
    params <- NULL
  }
  sql <- config_write_sql(
    con,
    run_table_ident(con, table),
    fields,
    if (upsert) table$keys else NULL,
    values
  )
  if (sqlite) {
    DBI::dbExecute(con, sql, params = unname(params))
  } else {
    DBI::dbExecute(con, sql)
  }
  return(invisible(NULL))
}

# The INSERT statement. `keys` NULL is a plain insert.
config_write_sql <- function(con, ident, fields, keys, values) {
  sql <- paste0(
    "INSERT INTO ",
    ident,
    " (",
    paste(run_qi(con, fields), collapse = ", "),
    ") VALUES ",
    values
  )
  if (is.null(keys)) {
    return(sql)
  }
  update <- setdiff(fields, keys)
  action <- "DO NOTHING"
  if (length(update) > 0L) {
    action <- paste0(
      "DO UPDATE SET ",
      paste0(
        run_qi(con, update),
        " = excluded.",
        run_qi(con, update),
        collapse = ", "
      )
    )
  }
  return(paste0(
    sql,
    " ON CONFLICT (",
    paste(run_qi(con, keys), collapse = ", "),
    ") ",
    action
  ))
}

# The VALUES list for PostgreSQL. Each value is a quoted literal, so the server
# casts it to the type of its column. NA becomes NULL.
config_sql_values <- function(con, rows) {
  cols <- lapply(rows, function(v) {
    return(run_q(con, as.character(v)))
  })
  return(paste0(
    "(",
    do.call(paste, c(unname(cols), sep = ", ")),
    ")",
    collapse = ", "
  ))
}
