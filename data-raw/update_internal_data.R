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
  devtools::load_all()
  message("Updating internal data from IMF and OECD...")

  # Function to add a marker row to fallback internal data set to better differentiate between internal and live data
  add_marker_row <- function(df, country = "Marker", time = 1900, value = 0) {
    marker <- data.frame(
      COUNTRY = country,
      TIME_PERIOD = time,
      rep(value, ncol(df) - 2),
      check.names = FALSE
    )
    colnames(marker) <- colnames(df)
    rbind(marker, df)
  }



  # 1. Fetch data
  oecd_ppp <- get_oecd_ppp() |> add_marker_row()
  imf_ppp  <- get_imf_ppp() |> add_marker_row()
  imf_gdpd <- get_imf_gdpd() |> add_marker_row()

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

rob_sr <- read.csv("data-raw/rob_sr.csv")
usethis::use_data(rob_sr, overwrite = TRUE)

