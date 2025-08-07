library(testthat)
library(httptest)

test_that("country_code_list returns a tibble with expected columns", {
  result <- country_code_list()
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("country.name.en", "iso3c") %in% names(result)))
  expect_gt(nrow(result), 0)
})

test_that("delfator_country_year_combs returns valid combinations for IMF", {
  with_mock_api({
    result <- delfator_country_year_combs(pppex_src = "IMF", use_live_data = FALSE)
    expect_s3_class(result, "data.frame")
    expect_true(all(c("country", "year", "country.name.en") %in% names(result)))
    expect_gt(nrow(result), 0)
  })
})

test_that("delfator_country_year_combs returns valid combinations for OECD", {
  with_mock_api({
    result <- delfator_country_year_combs(pppex_src = "OECD", use_live_data = FALSE)
    expect_s3_class(result, "data.frame")
    expect_true(all(c("country", "year", "country.name.en") %in% names(result)))
    expect_gt(nrow(result), 0)
  })
})

test_that("delfator_country_year_combs throws error for invalid pppex_src", {
  expect_error(
    delfator_country_year_combs(pppex_src = "INVALID"),
    regexp = "must be either \"IMF\" or \"OECD\""
  )
})

test_that("country_cleaner converts country names to ISO3C codes", {
  input <- c("Australia", "United States", "Germany")
  result <- country_cleaner(input)
  expect_type(result, "character")
  expect_equal(result, c("AUS", "USA", "DEU"))
})

test_that("country_cleaner returns NA for unknown country names", {
  input <- c("Atlantis", "Wakanda")
  result <- country_cleaner(input)
  expect_true(all(is.na(result)))
})
