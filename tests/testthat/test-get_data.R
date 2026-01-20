with_mock_api({
  test_that("creates temp directory if missing", {
    dir <- costDeflateR:::get_temp_data_dir()
    if (dir.exists(dir)) {
      unlink(dir, recursive = TRUE)
    }
    cond_update_internal_data(force = TRUE)
    expect_true(dir.exists(dir))
  })

  test_that("updates all datasets when force = TRUE", {
    dir <- costDeflateR:::get_temp_data_dir()
    cond_update_internal_data(force = TRUE)
    expect_true(file.exists(file.path(dir, "oecd_ppp.rds")))
    expect_true(file.exists(file.path(dir, "imf_ppp.rds")))
    expect_true(file.exists(file.path(dir, "imf_gdpd.rds")))
  })

  test_that("skips update if data is fresh and force = FALSE", {
    dir <- costDeflateR:::get_temp_data_dir()
    cond_update_internal_data(force = TRUE) # ensure files exist
    old_time <- Sys.time()
    Sys.setFileTime(file.path(dir, "oecd_ppp.rds"), old_time)
    Sys.setFileTime(file.path(dir, "imf_ppp.rds"), old_time)
    Sys.setFileTime(file.path(dir, "imf_gdpd.rds"), old_time)

    expect_no_warning(cond_update_internal_data(force = FALSE))
  })

  test_that("updates only selected datasets", {
    dir <- costDeflateR:::get_temp_data_dir()
    file.remove(file.path(dir, "oecd_ppp.rds"))
    file.remove(file.path(dir, "imf_ppp.rds"))
    file.remove(file.path(dir, "imf_gdpd.rds"))

    cond_update_internal_data(
      force = TRUE,
      dl_oecdppp = TRUE,
      dl_imfppp = FALSE,
      dl_imfgdpd = FALSE
    )
    expect_true(file.exists(file.path(dir, "oecd_ppp.rds")))
    expect_false(file.exists(file.path(dir, "imf_ppp.rds")))
    expect_false(file.exists(file.path(dir, "imf_gdpd.rds")))
  })
})


test_that("get_data returns correct structure for IMF source", {
  with_mock_api({
    tbl <- get_data(
      pppex_src = "IMF",
      use_live_data = TRUE,
      force_live_data = TRUE
    )
    expect_s3_class(tbl, "data.frame")
    expect_true(all(
      c("country", "year", "value_pppex", "value_gdpd") %in% names(tbl)
    ))
    expect_type(tbl$year, "double")
    expect_type(tbl$value_pppex, "double")
    expect_type(tbl$value_gdpd, "double")
  })
})

test_that("get_data returns correct structure for OECD source", {
  with_mock_api({
    tbl <- get_data(
      pppex_src = "OECD",
      use_live_data = TRUE,
      force_live_data = TRUE
    )
    expect_s3_class(tbl, "data.frame")
    expect_true(all(
      c("country", "year", "value_pppex", "value_gdpd") %in% names(tbl)
    ))
    expect_type(tbl$year, "double")
    expect_type(tbl$value_pppex, "double")
    expect_type(tbl$value_gdpd, "double")
  })
})

test_that("get_data uses fallback when offline", {
  without_internet({
    expect_warning(
      {
        tbl <- get_data(
          pppex_src = "IMF",
          use_live_data = TRUE,
          force_live_data = TRUE
        )
        expect_s3_class(tbl, "data.frame")
      },
      regexp = "Failed to fetch"
    )
  })
})

test_that("get_data uses internal data when use_live_data = FALSE", {
  expect_message(
    {
      tbl <- get_data(
        pppex_src = "OECD",
        use_live_data = FALSE,
        force_live_data = FALSE
      )
      expect_s3_class(tbl, "data.frame")
    },
    regexp = "Using internal data"
  )
})
