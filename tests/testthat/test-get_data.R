make_empty_dir <- function() {
  dir_empty <- file.path(tempdir(), "test-empty-dir")
  if (dir.exists(dir_empty)) {
    unlink(dir_empty, force = TRUE, recursive = TRUE)
  }
  dir.create(dir_empty, recursive = TRUE)
  dir_empty
}


with_mock_api({
  test_that("creates temp directory and downloads data if missing", {
    dir_empty <- make_empty_dir()
    cond_update_internal_data(force = TRUE, dir = dir_empty)
    expect_true(dir.exists(dir_empty))

    # updates all datasets when force = TRUE
    expect_snapshot(list.files(dir_empty))
  })

  dir <- dir_empty # now that it has data in it

  today <- Sys.Date()

  test_that("skips update if data is fresh and force = FALSE", {
    cond_update_internal_data(force = TRUE, dir = dir) # ensure files exist
    Sys.setFileTime(file.path(dir, "oecd_ppp.rds"), today)
    Sys.setFileTime(file.path(dir, "imf_ppp.rds"), today)
    Sys.setFileTime(file.path(dir, "imf_gdpd.rds"), today)

    # doesn't update recently created data
    expect_no_warning(cond_update_internal_data(force = FALSE))
  })

  test_that("updates only stale datasets", {
    Sys.setFileTime(file.path(dir, "imf_ppp.rds"), today - 10)
    expect_message(
      cond_update_internal_data(force = FALSE, dir = dir),
      regexp = "Updating IMF PPP data"
    )
  })

  test_that("updates only selected datasets", {
    file.remove(file.path(dir, "oecd_ppp.rds"))
    file.remove(file.path(dir, "imf_ppp.rds"))
    file.remove(file.path(dir, "imf_gdpd.rds"))

    cond_update_internal_data(
      dir = dir,
      force = TRUE,
      dl_oecdppp = TRUE,
      dl_imfppp = FALSE,
      dl_imfgdpd = FALSE
    )

    expect_identical(list.files(dir), "oecd_ppp.rds")
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
  # errors when there is no internet but force_live_data = TRUE
  suppressWarnings(
    without_internet(
      expect_error(
        get_data(
          pppex_src = "IMF",
          use_live_data = TRUE,
          force_live_data = TRUE
        ),
        regexp = "Use `force = FALSE` to use fallback data."
      )
    )
  )

  # It attempts to use fallback when use_live_data = TRUE and force_live_data = FALSE
  dir_empty <- file.path(tempdir(), "test-empty-dir")
  if (dir.exists(dir_empty)) {
    unlink(dir_empty, force = TRUE, recursive = TRUE)
  }
  dir.create(dir_empty)

  suppressWarnings(
    expect_warning(
      without_internet({
        tbl <- get_data(
          dir = dir_empty,
          pppex_src = "IMF",
          use_live_data = TRUE,
          force_live_data = FALSE
        )
        expect_s3_class(tbl, "data.frame")
      }),
      regexp = "Using internal fallback."
    )
  )
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
