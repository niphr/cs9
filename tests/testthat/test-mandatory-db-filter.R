d <- data.frame(
  granularity_time = c("day", "week", "day"),
  granularity_geo = c("nation", "nation", "nation"),
  country_iso3 = c("nor", "swe", "dnk"),
  location_code = c("a", "b", "c"),
  age = c("total", "total", "total"),
  sex = c("total", "total", "total"),
  stringsAsFactors = FALSE
)

test_that("country_iso3 includes only the matching row", {
  expect_equal(mandatory_db_filter(d, country_iso3 = "nor")$country_iso3, "nor")
})

test_that("location_code includes only the matching row", {
  expect_equal(mandatory_db_filter(d, location_code = "a")$location_code, "a")
})

test_that("granularity_time includes both matching rows", {
  expect_equal(
    mandatory_db_filter(d, granularity_time = "day")$location_code,
    c("a", "c")
  )
})

test_that("granularity_time_not excludes the matching rows", {
  expect_equal(
    mandatory_db_filter(d, granularity_time_not = "day")$location_code,
    "b"
  )
})

test_that("all arguments NULL returns the input unchanged", {
  expect_equal(mandatory_db_filter(d), d)
})
