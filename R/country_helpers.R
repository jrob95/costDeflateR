#' Country code list
#'
#' `country_code_list` returns a returns a list of available countries in the WEO data set.
#'
#' This is a helper function for checking allowable country mappings in the `countrycode` package between English and iso3c standards.
#'
#' @return Returns a \code{tibble}.
#' @export
country_code_list <- function() {
  tryCatch(
    {
      countrycode::codelist |> dplyr::select(country.name.en, iso3c)
    },
    error = function(e) {
      stop("country_code_list() failed: ", e$message)
    }
  )
}

#' IMF/ OECD data country/ year combo list
#'
#' `delfator_country_year_combs` returns a returns a list of available countriy/ year combinations in the WEO and/ or OECD PPPEX and defaltor data sets.
#'
#' This is a helper function for checking which country/ year combinations  exist in the reference data
#'
#' @param pppex_src A `character`, which dataset should PPP values come from? IMF or OECD?
#' @param use_live_data A `logical`. Makes call to `update_internal_data()`, if current IMF or OECD are more than a week old then make an API call to replace them., default = `TRUE`
#' @param force_live_data A `logical`. Makes call to `update_internal_data()` regardless of age of data currently stored. Use only if you know IMF WEO or OECD data has been updated since you last ran
#' @return Returns a \code{tibble}.
#' @export
delfator_country_year_combs <- function(
  pppex_src = "IMF",
  use_live_data = TRUE,
  force_live_data = FALSE
) {
  tryCatch(
    {
      if (!pppex_src %in% c("IMF", "OECD")) {
        stop("pppex_src must be either \"IMF\" or \"OECD\"")
      }

      tbl <- get_data(pppex_src, use_live_data, force_live_data) |>
        dplyr::select(country, year) |>
        dplyr::left_join(country_code_list(), dplyr::join_by(country == iso3c))
      return(tbl)
    },
    error = function(e) {
      stop("deflator_country_year_combs() failed: ", e$message)
    }
  )
}

#' Clean country names to match between IMF/ OECD ISO style country names and user input.
#'
#' `country_cleaner` recode country names form english to ISO3c format
#'
#' This is a helper function for clean up country names.
#'
#' @param data A `character`
country_cleaner <- function(data) {
  tryCatch(
    {
      countrycode::countrycode(
        data,
        origin = "country.name",
        destination = "iso3c"
      )
    },
    error = function(e) {
      stop("country_cleaner() failed: ", e$message)
    }
  )
}
