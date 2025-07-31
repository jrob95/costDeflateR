#' Prepare internal data in temporary directory
#'
#' Downloads OECD and IMF datasets and stores them in a temporary directory.
#' Falls back to internal data if download fails.
#'
#' @return Path to the temporary data directory
#' @export
update_internal_data <- function() {

  get_temp_data_dir <- function() {
    dir <- file.path(tempdir(), "costDeflateR_data")
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    dir
  }

  dir <- get_temp_data_dir()

  safe_fetch <- function(fetch_fn, fallback_name, filename) {
    tryCatch({
      data <- fetch_fn()
      saveRDS(data, file.path(dir, filename))
      message("Loaded live data: ", fallback_name)
    }, error = function(e) {
      warning("Failed to fetch ", fallback_name, ". Using internal fallback.")
      fallback <- get(fallback_name, envir = asNamespace("costDeflateR"))
      saveRDS(fallback, file.path(dir, filename))
    })
  }

  safe_fetch(get_oecd_ppp, "oecd_ppp", "oecd_ppp.rds")
  safe_fetch(get_imf_ppp,  "imf_ppp",  "imf_ppp.rds")
  safe_fetch(get_imf_gdpd, "imf_gdpd", "imf_gdpd.rds")

  options(costDeflateR.temp_data_dir = dir)
  invisible(dir)
}
