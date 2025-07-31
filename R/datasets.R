#' @docType data
#' @name   imf_ppp
#' @title IMF Purchasing Power Parity Values
#' @description A dataset containing purchasing power parity values for various countries and years from the IMF.
#' @format A tibble with 9,009 rows and 3 variables:
#' \describe{
#'   \item{COUNTRY}{Country name - WEO format}
#'   \item{YEAR}{Year}
#'   \item{PPPEX}{Purchasing power parity value}
#' }
#' @source <https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx>
"imf_ppp"
NULL

#' @docType data
#' @name imf_gdpd
#' @title IMF GDP Deflator Values
#' @description A dataset containing Gross Domestic Product Price Deflator index values for various countries and years.
#' @format A tibble with 9,292 rows and 3 variables:
#' \describe{
#'   \item{COUNTRY}{Country name - WEO format}
#'   \item{YEAR}{Year}
#'   \item{NGDP_D}{Gross Domestic Product Price Deflator index}
#' }
#' @source <https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx>
"imf_gdpd"
NULL

#' @docType data
#' @name   oecd_ppp
#' @title OECD Purchasing Power Parity Values
#' @description A dataset containing purchasing power parity values for various countries and years from the OECD
#' @format A tibble with 2,079 rows and 3 variables:
#' \describe{
#'   \item{COUNTRY}{Country name - WEO format}
#'   \item{YEAR}{Year}
#'   \item{PPP}{Purchasing power parity value}
#' }
#' @source <https://www.oecd.org/en/data/indicators/purchasing-power-parities-ppp.html>
"oecd_ppp"
NULL

#' @docType data
#' @name test_data
#' @title Test Data
#' @description A dataset containing test data for reference and testing.
#' @format A tibble with 23 rows and 5 variables:
#' \describe{
#'   \item{Reference}{Reference of study}
#'   \item{Field}{Name of field/cost}
#'   \item{country_orig}{Country name for cost}
#'   \item{year_orig}{Year of cost}
#'   \item{cost_orig}{Cost}
#' }
#' @source <https://doi.org/10.1016/j.chest.2023.06.040>
"test_data"
NULL
