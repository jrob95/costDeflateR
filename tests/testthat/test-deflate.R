library(testthat)
library(httptest)
library(dplyr)

test_that("deflate returns expected output with valid input", {
  with_mock_api({
    input_data <- data.frame(
      country_base = c("USA", "Germany"),
      year_base = c(2010, 2015),
      country_target = c("France", "Italy"),
      year_target = c(2015, 2020),
      cost_base = c(100, 200)
    )

    result <- deflate(
      input_data = input_data,
      cost_base = "cost_base",
      year_base = "year_base",
      country_base = "country_base",
      year_target = "year_target",
      country_target = "country_target",
      use_live_data = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true("cost_target" %in% names(result))
    expect_equal(nrow(result), 2)
  })
})

test_that("deflate throws error with invalid pppex_src", {
  expect_error(
    deflate(
      input_data = data.frame(cost_base = 100),
      cost_base = "cost_base",
      year_base = "2010",
      country_base = "USA",
      year_target = "2020",
      country_target = "France",
      pppex_src = "INVALID"
    ),
    regexp = "must be either \"IMF\" or \"OECD\""
  )
})

test_that("deflate throws error with non-numeric year", {
  input_data <- data.frame(
    country_base = "USA",
    year_base = "twenty-ten",
    country_target = "France",
    year_target = "2020",
    cost_base = 100
  )

  expect_error(
    deflate(
      input_data = input_data,
      cost_base = "cost_base",
      year_base = "year_base",
      country_base = "country_base",
      year_target = "year_target",
      country_target = "country_target"
    ),
    regexp = "must be numeric"
  )
})

test_that("deflate creates missing columns when values are constants", {
  input_data <- data.frame(cost_base = c(100, 200))

  result <- deflate(
    input_data = input_data,
    cost_base = "cost_base",
    year_base = "2010",
    country_base = "Australia",
    year_target = "2020",
    country_target = "USA",
    use_live_data = FALSE
  )

  expect_true("cost_target" %in% names(result))
})
