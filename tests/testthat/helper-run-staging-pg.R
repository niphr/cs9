# Fixtures for the PostgreSQL tests of the publish in batches, in
# test-run-staging-publish-batches.R.
#
# The tests run only when CS9_TEST_PG_SERVER names a PostgreSQL server.
# CS9_TEST_PG_PORT, CS9_TEST_PG_DB, CS9_TEST_PG_USER and CS9_TEST_PG_PASSWORD
# give the rest. The user MUST be able to create a schema in the database.
# Each test creates two schemas with a new name and drops them at its end.

skip_without_pg <- function() {
  testthat::skip_if(
    Sys.getenv("CS9_TEST_PG_SERVER") == "",
    "CS9_TEST_PG_SERVER is not set"
  )
}

# The connection of the test itself, through csdb, which cs9 imports.
pg_test_connect <- function() {
  return(csdb::DBConnection_v9$new(
    driver = "PostgreSQL Unicode",
    server = Sys.getenv("CS9_TEST_PG_SERVER"),
    port = as.integer(Sys.getenv("CS9_TEST_PG_PORT", "5432")),
    db = Sys.getenv("CS9_TEST_PG_DB"),
    user = Sys.getenv("CS9_TEST_PG_USER"),
    password = Sys.getenv("CS9_TEST_PG_PASSWORD")
  ))
}

# The config and anon accesses of cs9 in two new schemas of the test database.
# Returns the connection of the test, the anon dbconfig and the anon schema.
local_pg_db <- function(envir = parent.frame()) {
  conn <- pg_test_connect()
  admin <- conn$autoconnection
  tag <- paste0("cs9t_", Sys.getpid(), "_", sample.int(1e6, 1))
  schemas <- paste0(tag, c("_config", "_anon"))
  for (s in schemas) {
    DBI::dbExecute(admin, paste0("CREATE SCHEMA ", s))
  }
  withr::defer(
    {
      for (s in schemas) {
        DBI::dbExecute(admin, paste0("DROP SCHEMA IF EXISTS ", s, " CASCADE"))
      }
      conn$disconnect()
    },
    envir = envir
  )
  withr::local_envvar(
    c(
      CS9_AUTO = "0",
      CS9_PATH = withr::local_tempdir(.local_envir = envir),
      CS9_DBCONFIG_ACCESS = "config/anon",
      CS9_DBCONFIG_DRIVER = "PostgreSQL Unicode",
      CS9_DBCONFIG_SERVER = Sys.getenv("CS9_TEST_PG_SERVER"),
      CS9_DBCONFIG_PORT = Sys.getenv("CS9_TEST_PG_PORT", "5432"),
      CS9_DBCONFIG_USER = Sys.getenv("CS9_TEST_PG_USER"),
      CS9_DBCONFIG_PASSWORD = Sys.getenv("CS9_TEST_PG_PASSWORD"),
      CS9_DBCONFIG_ROLE_CREATE_TABLE = Sys.getenv("CS9_TEST_PG_USER"),
      CS9_DBCONFIG_DB_CONFIG = Sys.getenv("CS9_TEST_PG_DB"),
      CS9_DBCONFIG_SCHEMA_CONFIG = schemas[1],
      CS9_DBCONFIG_DB_ANON = Sys.getenv("CS9_TEST_PG_DB"),
      CS9_DBCONFIG_SCHEMA_ANON = schemas[2]
    ),
    .local_envir = envir
  )
  reload_db_config()
  withr::defer(
    for (tab in config$tables) {
      tab$disconnect()
    },
    envir = envir
  )
  return(list(
    admin = admin,
    dbconfig = config$dbconfigs$anon,
    schema = schemas[2]
  ))
}

# The live rows before the run, written with SQL outside any task run.
pg_old_rows <- function() {
  return(data.table::data.table(
    x = c(
      "old-p1-1",
      "old-p1-2",
      "keep-p1",
      "a-p1-upd",
      "old-p2-1",
      "keep-p2",
      "old-p3-1"
    ),
    part = c("p1", "p1", "p1", "p1", "p2", "p2", "p3"),
    n = c(0L, 0L, 5L, 5L, 0L, 5L, 0L)
  ))
}

pg_seed <- function(pg, out) {
  rows <- pg_old_rows()
  for (t in out$tables) {
    # tbl() creates the table at its first use.
    suppressMessages(invisible(t$tbl()))
    r <- rows[part == sub(".*_xxpxx_", "", t$table_name)]
    DBI::dbExecute(
      pg$admin,
      paste0(
        "INSERT INTO ",
        pg$schema,
        ".",
        t$table_name,
        " (x, part, n, auto_last_updated_datetime) VALUES ",
        paste0(
          "('",
          r$x,
          "', '",
          r$part,
          "', ",
          r$n,
          ", '2026-01-01 00:00:00')",
          collapse = ", "
        )
      )
    )
  }
  return(invisible(NULL))
}

# Three plans on the partitioned table of helper-run-staging.R:
#   plan 1 records drop_rows_where("n <= 2") and stages 10 rows in p3, n = 1.
#   plan 2 stages 2500 rows in p1 and 300 rows in p2, n = 2.
#   plan 3 upserts a-p1-upd and keep-p2 with n = 7.
# A second run of the operation after a batch would delete the published rows
# of plan 2. Plan 2 does not run again on a retry, so they would be lost.
local_pg_stage_ctl <- function(envir = parent.frame()) {
  ctl <- local_stage_ctl(envir)
  ctl$drop <- FALSE
  ctl$rows <- function(plan) {
    if (plan == 1) {
      return(data.table::data.table(
        x = sprintf("c-p3-%02d", 1:10),
        part = "p3",
        n = 1L
      ))
    }
    if (plan == 2) {
      return(data.table::data.table(
        x = c(sprintf("b-p1-%04d", 1:2500), sprintf("b-p2-%03d", 1:300)),
        part = rep(c("p1", "p2"), c(2500, 300)),
        n = 2L
      ))
    }
    return(NULL)
  }
  ctl$hooks <- list(
    "1" = function(tables, argset) {
      tables$out$drop_rows_where("n <= 2")
      return(invisible(NULL))
    },
    "3" = function(tables, argset) {
      tables$out$upsert_data(data.table::data.table(
        x = c("a-p1-upd", "keep-p2"),
        part = c("p1", "p2"),
        n = 7L
      ))
      return(invisible(NULL))
    }
  )
  return(ctl)
}

# The live rows after the run, built by hand: the old rows that the operation
# keeps, with the 2 upserts, and every staged row.
pg_expected <- function() {
  d <- data.table::data.table(
    x = c(
      "keep-p1",
      "a-p1-upd",
      sprintf("b-p1-%04d", 1:2500),
      "keep-p2",
      sprintf("b-p2-%03d", 1:300),
      sprintf("c-p3-%02d", 1:10)
    ),
    part = rep(c("p1", "p2", "p3"), c(2502, 301, 10)),
    n = c(5L, 7L, rep(2L, 2500), 7L, rep(2L, 300), rep(1L, 10))
  )
  data.table::setorderv(d, "x")
  return(d)
}

pg_staged_tables <- function(out) {
  return(c(
    vapply(out$tables, function(t) t$table_name, character(1)),
    "anon_stage_part_cs9stg",
    "cs9_run",
    "cs9_run_operation",
    "cs9_run_publish"
  ))
}

# Row triggers that record each insert, update and delete on the live tables,
# the staging table and the bookkeeping tables, with txid_current(). A
# transaction that rolls back takes its rows of cs9t_audit with it, so the
# table holds the committed transactions only. A table that does not exist
# gets no trigger.
pg_audit_install <- function(pg, out) {
  s <- pg$schema
  DBI::dbExecute(
    pg$admin,
    paste0(
      "CREATE TABLE IF NOT EXISTS ",
      s,
      ".cs9t_audit (txid bigint, tab text, op text, x text, part text, stg_id bigint)"
    )
  )
  DBI::dbExecute(
    pg$admin,
    paste0(
      "CREATE OR REPLACE FUNCTION ",
      s,
      ".cs9t_audit() RETURNS trigger LANGUAGE plpgsql AS $f$ ",
      "DECLARE r jsonb; BEGIN ",
      "IF TG_LEVEL = 'STATEMENT' THEN r := NULL; ",
      "ELSIF TG_OP = 'DELETE' THEN r := to_jsonb(OLD); ELSE r := to_jsonb(NEW); END IF; ",
      "INSERT INTO ",
      s,
      ".cs9t_audit VALUES (txid_current(), TG_TABLE_NAME, TG_OP, r ->> 'x', ",
      "r ->> 'part', CAST(r ->> 'cs9_stg_id' AS bigint)); ",
      "RETURN NULL; END $f$"
    )
  )
  for (tab in pg_staged_tables(out)) {
    exists <- DBI::dbGetQuery(
      pg$admin,
      paste0(
        "SELECT CASE WHEN to_regclass('",
        s,
        ".",
        tab,
        "') IS NULL THEN 0 ELSE 1 END AS e"
      )
    )$e
    if (as.integer(exists) == 1L) {
      DBI::dbExecute(
        pg$admin,
        paste0(
          "CREATE OR REPLACE TRIGGER cs9t_audit AFTER INSERT OR UPDATE OR DELETE ON ",
          s,
          ".",
          tab,
          " FOR EACH ROW EXECUTE FUNCTION ",
          s,
          ".cs9t_audit()"
        )
      )
      # A TRUNCATE fires no row trigger, so it gets a statement trigger.
      DBI::dbExecute(
        pg$admin,
        paste0(
          "CREATE OR REPLACE TRIGGER cs9t_audit_truncate AFTER TRUNCATE ON ",
          s,
          ".",
          tab,
          " FOR EACH STATEMENT EXECUTE FUNCTION ",
          s,
          ".cs9t_audit()"
        )
      )
    }
  }
  return(invisible(NULL))
}

# One row per committed transaction that wrote an audited table, in the order
# of txid: the staged rows it removed, the live rows it inserted and deleted,
# the live tables it truncated, the operations it removed from
# cs9_run_operation, and the updates of cs9_run.
pg_transactions <- function(pg, out) {
  a <- DBI::dbGetQuery(
    pg$admin,
    paste0("SELECT txid::text AS txid, tab, op FROM ", pg$schema, ".cs9t_audit")
  )
  live <- vapply(out$tables, function(t) t$table_name, character(1))
  ids <- sort(unique(as.numeric(a$txid)))
  count <- function(id, tab, op) {
    return(sum(as.numeric(a$txid) == id & a$tab %in% tab & a$op == op))
  }
  return(data.frame(
    txid = ids,
    staged = vapply(ids, count, numeric(1), "anon_stage_part_cs9stg", "DELETE"),
    inserted = vapply(ids, count, numeric(1), live, "INSERT"),
    live_deleted = vapply(ids, count, numeric(1), live, "DELETE"),
    truncated = vapply(ids, count, numeric(1), live, "TRUNCATE"),
    operations = vapply(ids, count, numeric(1), "cs9_run_operation", "DELETE"),
    published = vapply(ids, count, numeric(1), "cs9_run", "UPDATE")
  ))
}

# Does another session hold the advisory lock `name`? The test session tries
# to take it, and gives it back at once when it gets it.
pg_lock_is_held <- function(con, name) {
  key <- paste0("hashtext('", name, "')")
  got <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT CASE WHEN pg_try_advisory_lock(",
      key,
      ") THEN 1 ELSE 0 END AS got"
    )
  )$got
  if (as.integer(got) == 1L) {
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT CASE WHEN pg_advisory_unlock(",
        key,
        ") THEN 1 ELSE 0 END AS u"
      )
    )
    return(FALSE)
  }
  return(TRUE)
}

pg_count <- function(pg, sql) {
  return(as.numeric(DBI::dbGetQuery(pg$admin, sql)$n))
}

pg_staged_n <- function(pg) {
  return(pg_count(
    pg,
    paste0(
      "SELECT count(*)::int AS n FROM ",
      pg$schema,
      ".anon_stage_part_cs9stg"
    )
  ))
}

pg_published_n <- function(pg, run_key) {
  return(pg_count(
    pg,
    paste0(
      "SELECT count(*)::int AS n FROM ",
      pg$schema,
      ".cs9_run WHERE run_key = '",
      run_key,
      "' AND published_datetime IS NOT NULL"
    )
  ))
}

# The staged rows of the run, with the id that the database gave each row.
pg_staged_rows <- function(pg) {
  d <- DBI::dbGetQuery(
    pg$admin,
    paste0(
      "SELECT x, part, n, CAST(cs9_stg_id AS VARCHAR(20)) AS id FROM ",
      pg$schema,
      ".anon_stage_part_cs9stg"
    )
  )
  d <- data.table::as.data.table(d)
  d[, id := as.numeric(id)]
  data.table::setorderv(d, "id")
  return(d)
}

# The ids of the staged rows that each committed transaction removed from
# staging, by partition: a list with one element per transaction, in txid
# order.
pg_batch_ids <- function(pg) {
  a <- DBI::dbGetQuery(
    pg$admin,
    paste0(
      "SELECT CAST(txid AS VARCHAR(20)) AS txid, part, ",
      "CAST(stg_id AS VARCHAR(20)) AS id FROM ",
      pg$schema,
      ".cs9t_audit WHERE tab = 'anon_stage_part_cs9stg' AND op = 'DELETE'"
    )
  )
  ids <- sort(unique(as.numeric(a$txid)))
  return(lapply(ids, function(t) {
    d <- a[as.numeric(a$txid) == t, ]
    return(list(part = unique(d$part), id = sort(as.numeric(d$id))))
  }))
}

# The plan of each batch SELECT that the publish sends, from EXPLAIN on the
# connection of the test, at the moment the publish sends it.
local_batch_explain <- function(pg, envir = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$plans <- list()
  get_query <- DBI::dbGetQuery
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      if (startsWith(statement, "SELECT CAST(MAX(b.cs9_id)")) {
        plan <- get_query(pg$admin, paste("EXPLAIN", statement))[[1]]
        rec$plans[[length(rec$plans) + 1L]] <- paste(plan, collapse = " | ")
      }
      return(get_query(conn, statement, ...))
    },
    .package = "DBI",
    .env = envir
  )
  return(rec)
}

# The live tables that each committed TRUNCATE emptied, with its txid.
pg_truncates <- function(pg) {
  a <- DBI::dbGetQuery(
    pg$admin,
    paste0(
      "SELECT CAST(txid AS VARCHAR(20)) AS txid, tab FROM ",
      pg$schema,
      ".cs9t_audit WHERE op = 'TRUNCATE'"
    )
  )
  a$txid <- as.numeric(a$txid)
  return(a[order(a$txid), ])
}

# The scenario of local_pg_stage_ctl() with drop_all_rows() in plan 1, as HLM
# has it. The expected live rows are the staged rows only.
local_pg_drop_all_ctl <- function(envir = parent.frame()) {
  ctl <- local_pg_stage_ctl(envir)
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$out$drop_all_rows()
    return(invisible(NULL))
  }
  return(ctl)
}

pg_expected_drop_all <- function() {
  return(pg_expected()[x != "keep-p1"])
}

pg_staging_columns <- function(pg) {
  return(
    DBI::dbGetQuery(
      pg$admin,
      paste0(
        "SELECT column_name FROM information_schema.columns WHERE ",
        "table_schema = '",
        pg$schema,
        "' AND table_name = 'anon_stage_part_cs9stg'"
      )
    )$column_name
  )
}

pg_staging_indexes <- function(pg) {
  return(
    DBI::dbGetQuery(
      pg$admin,
      paste0(
        "SELECT indexdef FROM pg_indexes WHERE schemaname = '",
        pg$schema,
        "' AND tablename = 'anon_stage_part_cs9stg'"
      )
    )$indexdef
  )
}

# Gives the staging table, with its rows, the shape that cs9 3951f7f gave it:
# no cs9_stg_id, and the index cs9_run on cs9_task, cs9_run_key and part.
pg_make_old_staging <- function(pg) {
  tab <- paste0(pg$schema, ".anon_stage_part_cs9stg")
  DBI::dbExecute(pg$admin, paste0("DROP TRIGGER IF EXISTS cs9_stg_id ON ", tab))
  indexes <- DBI::dbGetQuery(
    pg$admin,
    paste0(
      "SELECT CAST(CAST(indexrelid AS regclass) AS VARCHAR) AS name FROM pg_index ",
      "WHERE NOT indisprimary AND indrelid = to_regclass('",
      tab,
      "')"
    )
  )$name
  for (i in indexes) {
    DBI::dbExecute(pg$admin, paste0("DROP INDEX ", i))
  }
  DBI::dbExecute(
    pg$admin,
    paste0("ALTER TABLE ", tab, " DROP COLUMN cs9_stg_id")
  )
  DBI::dbExecute(
    pg$admin,
    paste0(
      "CREATE INDEX cs9t_old_cs9_run ON ",
      tab,
      " (cs9_task, cs9_run_key, part)"
    )
  )
  return(invisible(NULL))
}

# Does another session hold the publish lock of every partition of `out`?
pg_live_locks_held <- function(pg, out) {
  return(all(vapply(
    out$tables,
    function(t) pg_lock_is_held(pg$admin, paste("cs9 table", t$table_name)),
    logical(1)
  )))
}
