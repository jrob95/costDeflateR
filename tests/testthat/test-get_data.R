httptest::with_mock_api({
  test_that("update_internal_data writes all expected files", {
    # Run the function
    temp_dir <- base::suppressWarnings(update_internal_data())

    # Check that files exist
    expect_true(file.exists(file.path(temp_dir, "oecd_ppp.rds")))
    expect_true(file.exists(file.path(temp_dir, "imf_ppp.rds")))
    expect_true(file.exists(file.path(temp_dir, "imf_gdpd.rds")))

    # Check that data is readable
    expect_s3_class(readRDS(file.path(temp_dir, "oecd_ppp.rds")), "data.frame")
    expect_s3_class(readRDS(file.path(temp_dir, "imf_ppp.rds")), "data.frame")
    expect_s3_class(readRDS(file.path(temp_dir, "imf_gdpd.rds")), "data.frame")
  })
})

test_that("fallback is triggered on error", {
  # Mock one fetch function to throw an error
  with_mocked_bindings(
    get_oecd_ppp = function() stop("404 error"),
    {
      temp_dir <- update_internal_data()

      # Should still write fallback file
      expect_true(file.exists(file.path(temp_dir, "oecd_ppp.rds")))

      # Should contain internal fallback data
      fallback <- get("oecd_ppp", envir = asNamespace("costDeflateR"))
      loaded   <- readRDS(file.path(temp_dir, "oecd_ppp.rds"))
      expect_equal(loaded, fallback)
    }
  )
})

test_that("messages and warnings are emitted", {
  expect_message(update_internal_data(), "Loaded live data")
  expect_warning(
    with_mocked_bindings(get_imf_ppp = function() stop("fail"),
              update_internal_data()),
    "Failed to fetch imf_ppp"
  )
})

test_that("temporary directory is cleaned up", {
  temp_dir <- update_internal_data()
  unlink(temp_dir, recursive = TRUE)
  expect_false(dir.exists(temp_dir))
})

