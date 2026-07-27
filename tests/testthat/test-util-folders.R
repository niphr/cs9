local_config_path <- function(env = parent.frame()) {
  root <- fs::path(tempfile("cs9-path-"))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  old <- config$path
  config$path <- root
  withr::defer(config$path <- old, envir = env)
  root
}

test_that("path() composes the documented path when auto = TRUE", {
  root <- local_config_path()

  expect_equal(path("a", auto = TRUE), paste0(root, "/a"))
  expect_equal(path("a", "b", auto = TRUE), paste0(root, "/a/b"))
  expect_equal(
    path("a", trailing_slash = TRUE, auto = TRUE),
    paste0(root, "/a/")
  )
})

test_that("path() splices _interactive into position two when auto = FALSE", {
  root <- local_config_path()

  expect_equal(path("a", auto = FALSE), paste0(root, "/a/_interactive"))
  expect_equal(path("a", "b", auto = FALSE), paste0(root, "/a/_interactive/b"))
  expect_equal(
    path("a", "b", "c", auto = FALSE),
    paste0(root, "/a/_interactive/b/c")
  )
})

test_that("create_folder_if_doesnt_exist() creates recursively and is idempotent", {
  root <- local_config_path()
  target <- paste0(root, "/x/y")

  expect_false(fs::dir_exists(target))
  expect_equal(as.character(create_folder_if_doesnt_exist(target)), target)
  expect_true(fs::dir_exists(target))
  expect_equal(as.character(create_folder_if_doesnt_exist(target)), target)
  expect_true(fs::dir_exists(target))
})
