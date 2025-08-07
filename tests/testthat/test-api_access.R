library(testthat)
library(httptest)
library(dplyr)

# with_mock_api({
#   test_that("get_oecd_ppp returns expected structure", {
#     data <- get_oecd_ppp()
#     expect_s3_class(data, "data.frame")
#     expect_true(all(c("COUNTRY", "TIME_PERIOD", "PPP") %in% names(data)))
#     expect_type(data$TIME_PERIOD, "double")
#   })
#
#
#   # test_that("get_imf returns expected structure", {
#   #   key <- "*.NGDP_D.*"
#   #   data <- get_imf(key)
#   #   expect_s3_class(data, "data.frame")
#   #   expect_true("TIME_PERIOD" %in% names(data))
#   #   expect_type(data$TIME_PERIOD, "double")
#   # })
#
#   test_that("get_imf_gdpd returns expected structure", {
#     data <- get_imf_gdpd()
#     expect_s3_class(data, "data.frame")
#     expect_true(all(c("COUNTRY", "TIME_PERIOD", "NGDP_D") %in% names(data)))
#     expect_type(data$TIME_PERIOD, "double")
#   })
#
#   test_that("get_imf_ppp returns expected structure", {
#     data <- get_imf_ppp()
#     expect_s3_class(data, "data.frame")
#     expect_true(all(c("COUNTRY", "TIME_PERIOD", "PPPEX") %in% names(data)))
#     expect_type(data$TIME_PERIOD, "double")
#   })
# })
#
# library(testthat)
# library(httptest)

with_mock_api({
  test_that("get_oecd_ppp returns expected structure", {
    data <- get_oecd_ppp()
    expect_s3_class(data, "data.frame")
    expect_named(data, c("COUNTRY", "TIME_PERIOD", "PPP"))
    expect_type(data$TIME_PERIOD, "double")
    expect_type(data$PPP, "double")
    expect_true(all(!is.na(data$PPP)))
  })

  test_that("get_imf_gdpd returns expected structure", {
    data <- get_imf_gdpd()
    expect_s3_class(data, "data.frame")
    expect_named(data, c("COUNTRY", "TIME_PERIOD", "NGDP_D"))
    expect_type(data$TIME_PERIOD, "double")
    expect_type(data$NGDP_D, "double")
  })

  test_that("get_imf_ppp returns expected structure", {
    data <- get_imf_ppp()
    expect_s3_class(data, "data.frame")
    expect_named(data, c("COUNTRY", "TIME_PERIOD", "PPPEX"))
    expect_type(data$TIME_PERIOD, "double")
    expect_type(data$PPPEX, "double")
    expect_true(all(!is.na(data$PPPEX)))
  })
})
