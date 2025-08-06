#' Conditionally update internal data
#'
#' Checks the age of each dataset in the temporary directory and updates only those
#' that are missing or older than 7 days, or if `force` is set to TRUE.
#'
#' @param force Logical. If TRUE, forces the update regardless of data age.
#' @param dl_oecdppp Logical. Whether to check/update OECD PPP data.
#' @param dl_imfppp Logical. Whether to check/update IMF PPP data.
#' @param dl_imfgdpd Logical. Whether to check/update IMF GDP deflator data.
#' @return Path to the temporary data directory
cond_update_internal_data <- function(force = FALSE, dl_oecdppp = TRUE, dl_imfppp = TRUE, dl_imfgdpd = TRUE) {
  is_stale <- function(file_path) {
    if (!file.exists(file_path)) {
      return(TRUE)
    }
    age_days <- as.numeric(difftime(Sys.time(), file.info(file_path)$mtime, units = "days"))
    age_days > 7
  }

  dir <- get_temp_data_dir()
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Define file paths
  files <- list(
    oecd_ppp  = file.path(dir, "oecd_ppp.rds"),
    imf_ppp   = file.path(dir, "imf_ppp.rds"),
    imf_gdpd  = file.path(dir, "imf_gdpd.rds")
  )

  # Determine which files need updating
  update_flags <- c(
    oecd_ppp  = dl_oecdppp && (force || is_stale(files$oecd_ppp)),
    imf_ppp   = dl_imfppp && (force || is_stale(files$imf_ppp)),
    imf_gdpd  = dl_imfgdpd && (force || is_stale(files$imf_gdpd))
  )

  # Update only the stale or missing datasets
  if (update_flags["oecd_ppp"]) {
    message("Updating OECD PPP data...")
    safe_fetch(get_oecd_ppp, "oecd_ppp", "oecd_ppp.rds", dir)
  }
  if (update_flags["imf_ppp"]) {
    message("Updating IMF PPP data...")
    safe_fetch(get_imf_ppp, "imf_ppp", "imf_ppp.rds", dir)
  }
  if (update_flags["imf_gdpd"]) {
    message("Updating IMF GDPD data...")
    safe_fetch(get_imf_gdpd, "imf_gdpd", "imf_gdpd.rds", dir)
  }

  # options(costDeflateR.temp_data_dir = dir)
  # invisible(dir)
}


#' Prepare internal data in temporary directory
#'
#' Downloads OECD and IMF datasets and stores them in a temporary directory.
#' Falls back to internal data if download fails.
#' @param fetch_fn function to use to fetch data from api
#' @param fallback_name name of internal dataset to use as fallback
#' @param filename name of file in temp folder.
#'
#' @return Path to the temporary data directory
safe_fetch <- function(fetch_fn, fallback_name, filename, dir) {
  tryCatch(
    {
      data <- fetch_fn()
      saveRDS(data, file.path(dir, filename))
      message("Loaded live data: ", fallback_name)
    },
    error = function(e) {
      warning("Failed to fetch ", fallback_name, ". Using internal fallback.")
      use_internal_data(fallback_name, filename)
    }
  )
}


use_internal_data <- function(fallback_name, filename) {
  dir <- file.path(tempdir(), "costDeflateR_data")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  fallback <- get(fallback_name, envir = asNamespace("costDeflateR"))
  saveRDS(fallback, file.path(dir, filename))
}

get_temp_data_dir <- function() {
  file.path(tempdir(), "costDeflateR_data")
}

#----- get data
#' Get data
#'
#' `get_data` Checks user preference for source of OECD/ IMF PPPEX and GDPD data (internal or live) and loads data accordingly.
#'
#' @param pppex_src A `character`, which dataset should PPP values come from? IMF or OECD?
#' @param use_live_data A `logical`. Makes call to `update_internal_data()`, if current IMF or OECD are more than a week old then make an API call to replace them., default = `TRUE`
#' @param force_live_data A `logical`. Makes call to `update_internal_data()` regardless of age of data currently stored. Use only if you know IMF WEO or OECD data has been updated since you last ran
get_data <- function(pppex_src, use_live_data, force_live_data) {
  # deal with internal/ live data.
  dl_imfppp <- dl_oecdppp <- FALSE

  if (pppex_src == "IMF") dl_imfppp <- TRUE
  if (pppex_src == "OECD") dl_oecdppp <- TRUE
  # get live data, store temp.
  if (use_live_data) {
    message("Attempting to use live data from IMF/ OECD")
    cond_update_internal_data(force_live_data, dl_oecdppp, dl_imfppp)
    dir <- get_temp_data_dir()
    if (dl_oecdppp == TRUE) {
    oecd_ppp <- readRDS(file.path(dir, "oecd_ppp.rds"))}
    if (dl_imfppp == TRUE) {
    imf_ppp <- readRDS(file.path(dir, "imf_ppp.rds"))}
    if (TRUE) {
    imf_gdpd <- readRDS(file.path(dir, "imf_gdpd.rds"))}
  } else {
    message(paste0("Using internal data. Last upated", update_meta[updated_at]))
  }

  # point data to correct dataset.
  if (pppex_src == "IMF") {
    ppp_vals <- imf_ppp |>
      dplyr::select(country = COUNTRY, year = TIME_PERIOD, value = PPPEX)
  }
  if (pppex_src == "OECD") {
    ppp_vals <- oecd_ppp |>
      dplyr::select(country = COUNTRY, year = TIME_PERIOD, value = PPP)
  }

  if (TRUE) {
    gdpd_vals <- imf_gdpd |>
      dplyr::select(country = COUNTRY, year = TIME_PERIOD, value = NGDP_D)
  }

  # country combs for later
  tbl <- ppp_vals |>
    dplyr::inner_join(gdpd_vals, dplyr::join_by(country, year), suffix = c("_pppex", "_gdpd"))
  return(tbl)
}
