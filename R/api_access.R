# Helper functions for accessing datasets through OECD & IMF APIs ####

#----- OECD PPP exchange
#' Get OECD PPP values
#'
#' @description
#' Get Purchasing power parity (PPP) values from OECD data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant OECD PPP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a></a> for more information
#'
#' @usage NULL
#'
get_oecd_ppp <- function() {
  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/",
    "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,1.0/",
    "A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...",
    "PPP_B1GQ.......?dimensionAtObservation=AllDimensions"
  )
  cat("[rsdmx][INFO] Fetching \'https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,1.0/A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...PPP_B1GQ.......?dimensionAtObservation=AllDimensions\'")

  data <- rsdmx::readSDMX(url) |>
    as.data.frame(data) |>
    dplyr::select(COUNTRY = .data$REF_AREA, .data$TIME_PERIOD, PPP = .data$obsValue) |>
    dplyr::mutate(TIME_PERIOD = as.numeric(.data$TIME_PERIOD))


  return(data)
}

#----- IMF data
#' Get IMF data
#'
#' @description
#' Get helper function for accessing IMF data through SMDX API.
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF PPPEX & GDP deflator values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a>placeholderlink</a> for more information
#'
#' @usage NULL
get_imf <- function(key) {
  data <- as.data.frame(rsdmx::readSDMX(
    providerId = "IMF_DATA",
    resource = "data",
    flowRef = "IMF.RES,WEO",
    key = key
  )) |>
    dplyr::mutate(TIME_PERIOD = as.numeric(.data$TIME_PERIOD))
  return(data)
}


#----- IMF gdp_d
#' Get IMF GDP_D values
#'
#' @description
#' Get gross domestic product, deflator (GDP_D) values from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF GDP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a></a> for more information
#'
#' @usage NULL
#'
get_imf_gdpd <- function() {
  get_imf("*.NGDP_D.*") |>
    dplyr::select(.data$COUNTRY, .data$TIME_PERIOD, NGDP_D = .data$OBS_VALUE)
}

#----- IMF ppp
#' Get IMF GDP_D values
#'
#' @description
#' Get Implied PPP conversion rate (PPPEX) values from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF PPPEX values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a></a> for more information
#'
#' @usage NULL
get_imf_ppp <- function() {
  get_imf("*.PPPEX.*") |>
    dplyr::select(.data$COUNTRY, .data$TIME_PERIOD, PPPEX = .data$OBS_VALUE)
}
