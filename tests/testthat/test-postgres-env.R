# validate_environment() only reads environment variables, so these tests
# connect to nothing.
postgres_env <- function(dir) {
  c(
    CS9_AUTO = "0",
    CS9_PATH = dir,
    CS9_DBCONFIG_ACCESS = "config/anon",
    CS9_DBCONFIG_DRIVER = "PostgreSQL Unicode",
    CS9_DBCONFIG_SERVER = "localhost",
    CS9_DBCONFIG_PORT = "5432",
    CS9_DBCONFIG_USER = "cs9_user",
    CS9_DBCONFIG_SCHEMA_CONFIG = "config",
    CS9_DBCONFIG_DB_CONFIG = "cs9_surveillance",
    CS9_DBCONFIG_SCHEMA_ANON = "anon",
    CS9_DBCONFIG_DB_ANON = "cs9_surveillance"
  )
}

test_that("a PostgreSQL environment without CS9_DBCONFIG_PASSWORD validates as ok", {
  d <- withr::local_tempdir()
  withr::local_envvar(c(postgres_env(d), CS9_DBCONFIG_PASSWORD = NA))
  expect_identical(Sys.getenv("CS9_DBCONFIG_PASSWORD"), "")

  result <- validate_environment()
  expect_equal(result$status, "ok")
  expect_equal(result$issues, character(0))
})

test_that("every other required PostgreSQL variable is still required", {
  d <- withr::local_tempdir()
  env <- postgres_env(d)
  for (var in names(env)) {
    unset_one <- env
    unset_one[[var]] <- NA_character_
    withr::with_envvar(
      c(unset_one, CS9_DBCONFIG_PASSWORD = NA),
      {
        result <- validate_environment()
        expect_equal(result$status, "error", label = var)
        expect_true(
          any(grepl(var, result$issues, fixed = TRUE)),
          label = var
        )
      }
    )
  }
})
