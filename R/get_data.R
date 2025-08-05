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
#' @export
cond_update_internal_data <- function(force = FALSE, dl_oecdppp = TRUE, dl_imfppp = TRUE, dl_imfgdpd = TRUE) {


  is_stale <- function(file_path) {
    if (!file.exists(file_path)) return(TRUE)
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
    oecd_ppp  = dl_oecdppp  && (force || is_stale(files$oecd_ppp)),
    imf_ppp   = dl_imfppp   && (force || is_stale(files$imf_ppp)),
    imf_gdpd  = dl_imfgdpd  && (force || is_stale(files$imf_gdpd))
  )

  # Update only the stale or missing datasets
  if (update_flags["oecd_ppp"]) {
    message("Updating OECD PPP data...")
    safe_fetch(get_oecd_ppp, "oecd_ppp", "oecd_ppp.rds")
  }
  if (update_flags["imf_ppp"]) {
    message("Updating IMF PPP data...")
    safe_fetch(get_imf_ppp, "imf_ppp", "imf_ppp.rds")
  }
  if (update_flags["imf_gdpd"]) {
    message("Updating IMF GDPD data...")
    safe_fetch(get_imf_gdpd, "imf_gdpd", "imf_gdpd.rds")
  }

  options(costDeflateR.temp_data_dir = dir)
  invisible(dir)
}


#' Prepare internal data in temporary directory
#'
#' Downloads OECD and IMF datasets and stores them in a temporary directory.
#' Falls back to internal data if download fails.
#'
#' @return Path to the temporary data directory
#' @export
# update_internal_data <- function(dl_oecdppp = TRUE, dl_imfppp = TRUE, dl_imfgdpd = TRUE) {
#
#   get_temp_data_dir <- function() {
#     dir <- file.path(tempdir(), "costDeflateR_data")
#     if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
#     dir
#   }
#
#   dir <- get_temp_data_dir()

  safe_fetch <- function(fetch_fn, fallback_name, filename) {
    tryCatch({
      data <- fetch_fn()
      saveRDS(data, file.path(dir, filename))
      message("Loaded live data: ", fallback_name)
    }, error = function(e) {
      warning("Failed to fetch ", fallback_name, ". Using internal fallback.")
      use_internal_data(fallback_name, filename)
    })
  }
#   if (dl_oecdppp)  safe_fetch(get_oecd_ppp, "oecd_ppp", "oecd_ppp.rds")
#   if (dl_imfppp)  safe_fetch(get_imf_ppp,  "imf_ppp",  "imf_ppp.rds")
#   if (dl_imfgdpd)  safe_fetch(get_imf_gdpd, "imf_gdpd", "imf_gdpd.rds")
#
#   options(costDeflateR.temp_data_dir = dir)
#   invisible(dir)
# }

#' Conditionally update internal data
#'
#' Checks the age of the temporary data directory and updates it if older than 7 days,
#' or if `force` is set to TRUE.
#'
#' @param force Logical. If TRUE, forces the update regardless of data age. Default is FALSE.
#' @param dl_oecd Logical. If TRUE,
#' @param dl_imfLogical. If TRUE,
#' @param dl_imfgdpd
#' @return Path to the temporary data directory
#' @export
# cond_update_internal_data <- function(force = FALSE, dl_oecdppp = TRUE, dl_imfppp = TRUE, dl_imfgdpd = TRUE) {
#   get_temp_data_dir <- function() {
#     file.path(tempdir(), "costDeflateR_data")
#   }
#
#   dir <- get_temp_data_dir()
#   data_exists <- dir.exists(dir)
#
#   # Check modification time
#   is_stale <- function(path) {
#     if (!file.exists(path)) return(TRUE)
#     age_days <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
#     age_days > 7
#   }
#
#   if (force || !data_exists || is_stale(dir)) {
#     message("Updating internal data...")
#     update_internal_data(dl_oecdppp, dl_imfppp, dl_imfgdpd)
#   } else {
#     message("Using cached data from: ", dir)
#   }
#
#   invisible(dir)
# }

use_internal_data <- function(fallback_name, filename){
  dir <- file.path(tempdir(), "costDeflateR_data")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  fallback <- get(fallback_name, envir = asNamespace("costDeflateR"))
  saveRDS(fallback, file.path(dir, filename))
}

get_temp_data_dir <- function() {
  file.path(tempdir(), "costDeflateR_data")
}
