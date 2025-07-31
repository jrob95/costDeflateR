#' Update internal data from IMF and OECD sources
#'
#' @description
#' This internal function is intended for package developers only. It refreshes
#' the internal datasets used for deflation and PPP conversion by pulling the
#' latest values from IMF and OECD SDMX endpoints.
#'
#' @details
#' This function is not exported and should not be called by end users. It is
#' used during package development to ensure that internal datasets are current.
#' The function also stores a timestamp of the last update for provenance.
#'
#' @keywords internal
#' @usage NULL
#' @return Invisibly returns a list of updated datasets
#' @noRd
dev_update_data <- function() {
  message("Updating internal data from IMF and OECD...")

  # 1. Fetch data
  oecd_ppp <- get_oecd_ppp()
  imf_ppp  <- get_imf_ppp()
  imf_gdpd <- get_imf_gdpd()

  # 2. Save datasets
  usethis::use_data(oecd_ppp, overwrite = TRUE)
  usethis::use_data(imf_ppp,  overwrite = TRUE)
  usethis::use_data(imf_gdpd, overwrite = TRUE)

  # 3. Save timestamp
  update_meta <- list(
    updated_at = Sys.time(),
    updated_by = Sys.info()[["user"]],
    sources = list(
      oecd_ppp_key = "PPP_B1GQ",
      imf_ppp_key  = "*.PPPGDP.*",
      imf_gdpd_key = "*.NGDP_D.*"
    )
  )
  usethis::use_data(internal = TRUE, update_meta, overwrite = TRUE)

  invisible(list(
    oecd_ppp = oecd_ppp,
    imf_ppp  = imf_ppp,
    imf_gdpd = imf_gdpd,
    update_meta = update_meta
  ))
}
dev_update_data()

test_data <- read.csv("data-raw/test_data.csv")
usethis::use_data(test_data, overwrite = TRUE)

