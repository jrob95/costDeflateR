with_mock_api({
  test_that("get_oecd_ppp returns expected structure", {
    skip_if_offline()
    skip_on_cran()

    data <- get_oecd_ppp()
    expect_s3_class(data, "data.frame")
    expect_named(data, c("COUNTRY", "TIME_PERIOD", "PPP"))
    expect_type(data$TIME_PERIOD, "double")
    expect_type(data$PPP, "double")
    expect_true(all(!is.na(data$PPP)))
  })

  test_that("get_imf_gdpd returns expected structure", {
    skip_if_offline()
    skip_on_cran()

    data <- get_imf_gdpd()
    expect_s3_class(data, "data.frame")
    expect_named(data, c("COUNTRY", "TIME_PERIOD", "NGDP_D"))
    expect_type(data$TIME_PERIOD, "double")
    expect_type(data$NGDP_D, "double")
  })

  test_that("get_imf_ppp returns expected structure", {
    skip_if_offline()
    skip_on_cran()

    data <- get_imf_ppp()
    expect_s3_class(data, "data.frame")
    expect_named(data, c("COUNTRY", "TIME_PERIOD", "PPPEX"))
    expect_type(data$TIME_PERIOD, "double")
    expect_type(data$PPPEX, "double")
    expect_true(all(!is.na(data$PPPEX)))
  })
})

test_that("fails without internet", {
  local_mocked_bindings(is_internet_down = function(...) TRUE)
  expect_error(get_oecd_ppp(), "Check your internet")
  expect_error(get_imf_gdpd(), "Check your internet")
  expect_error(get_imf_ppp(), "Check your internet")
})
