# Helper functions for accessing datasets through OECD & IMF APIs ####

#----- OECD PPP exchange
#' Get OECD PPP values
#'
#' @description
#' Get Purchasing power parity (PPP) values from OECD data sets.
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant OECD PPP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a href = "https://www.oecd.org/en/data/indicators/purchasing-power-parities-ppp.html"> OECD website</a> for more information.
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

  tryCatch(
    {
      data <- rsdmx::readSDMX(url) |>
        as.data.frame(data) |>
        dplyr::select(COUNTRY = REF_AREA, TIME_PERIOD, PPP = obsValue) |>
        dplyr::mutate(TIME_PERIOD = as.numeric(TIME_PERIOD))

      return(data)
    },
    error = function(e) {
      warning("Failed to fetch OECD PPP data: ", conditionMessage(e))
      return(NULL)
    }
  )
}


#----- IMF data
#' Get IMF data
#'
#' @description
#' Get helper function for accessing IMF data through SMDX API.
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF PPPEX & GDP deflator values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a href = "https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending">IMF WEO website</a> for more information.
#'
#' @usage NULL
get_imf <- function(key) {
  tryCatch(
    {
      data <- as.data.frame(rsdmx::readSDMX(
        providerId = "IMF_DATA",
        resource = "data",
        flowRef = "IMF.RES,WEO",
        key = key
      )) |>
        dplyr::mutate(
          TIME_PERIOD = as.numeric(TIME_PERIOD),
          OBS_VALUE = as.numeric(OBS_VALUE)
        )
      return(data)
    },
    error = function(e) {
      warning("Failed to fetch IMF data: ", conditionMessage(e))
      return(NULL)
    }
  )
}



#----- IMF gdp_d
#' Get IMF GDP_D values
#'
#' @description
#' Get gross domestic product, deflator (GDP_D) values from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF GDP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a href = "https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending">IMF WEO website</a> for more information.
#'
#' @usage NULL
#'
get_imf_gdpd <- function() {
  get_imf("*.NGDP_D.*") |>
    dplyr::select(COUNTRY, TIME_PERIOD, NGDP_D = OBS_VALUE)
}

#----- IMF ppp
#' Get IMF GDP_D values
#'
#' @description
#' Get Implied PPP conversion rate (PPPEX) values from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF PPPEX values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a href = "https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending">IMF WEO website</a> for more information.
#'
#' @usage NULL
get_imf_ppp <- function() {
  get_imf("*.PPPEX.*") |>
    dplyr::select(COUNTRY, TIME_PERIOD, PPPEX = OBS_VALUE)
}
