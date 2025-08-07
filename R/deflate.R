#' Deflate currency values
#'
#' @description
#' `deflate()` returns a dataframe with deflated cost values, optionally adding up to five new columns depending on the input structure.
#'
#' @details
#' This function deflates currency values using GDP deflators and PPP exchange rates.
#' You can either specify a base and target country/year for each row via column names,
#' or provide constant values to apply across all rows.
#'
#' @param input_data A `data.frame` containing the data to be deflated.
#' @param country_base A `character` string: either a country name or the name of a column in `input_data` containing base countries.
#' @param year_base A `numeric` or `character`: either a year or the name of a column in `input_data` containing base years.
#' @param year_target A `numeric` or `character`: either a year or the name of a column in `input_data` containing target years.
#' @param country_target A `character`: either a country name or the name of a column in `input_data` containing target countries.
#' @param cost_base A `character`: the name of the column in `input_data` containing base cost values.
#' @param cost_target A `character`: the name for the new column that will contain deflated cost values. Defaults to `"cost_target"`.
#' @param rename_countries A `logical`: whether to standardize country names to ISO3C format. Defaults to `TRUE`.
#' @param pppex_src A `character`: the source of PPP data, either `"IMF"` or `"OECD"`.
#' @param use_live_data A `logical`: if `TRUE`, updates internal data if older than one week. Defaults to `TRUE`.
#' @param force_live_data A `logical`: if `TRUE`, forces an update of internal data regardless of age. Defaults to `FALSE`.
#'
#' @return A `data.frame` with the original data and an additional column (named by `cost_target`) containing deflated cost values.
#'
#' @examples
#' # Example 1: Using column names for base and target values
#' data <- data.frame(
#'   country_base = c("USA", "Germany"),
#'   year_base = c(2010, 2015),
#'   country_target = c("France", "Italy"),
#'   year_target = c(2015, 2020),
#'   cost_base = c(100, 200)
#' )
#'
#' # Deflate currency values
#' deflated_data <- deflate(
#'   input_data = data,
#'   cost_base = "cost_base",
#'   year_base = "year_base",
#'   country_base = "country_base",
#'   year_target = "year_target",
#'   country_target = "country_target"
#' )
#'
#' # You can also also specify values in the function for year and country
#' data <- data.frame(
#'   cost_base = c(100, 200)
#' )
#'
#' # Example 2: Using constant values for base and target
#' deflated_data <- deflate(
#'   input_data = data,
#'   cost_base = "cost_base",
#'   year_base = "2010",
#'   country_base = "Australia",
#'   year_target = "2020",
#'   country_target = "United States",
#'   rename_countries = TRUE
#' )
#'
#' @export
deflate <- function(
    input_data,
    cost_base,
    year_base,
    country_base,
    year_target,
    country_target = "USA",
    cost_target = "cost_target",
    pppex_src = "IMF",
    rename_countries = TRUE,
    use_live_data = TRUE,
    force_live_data = FALSE) {
  tryCatch(
    {
      # Validate pppex_src
      if (!pppex_src %in% c("IMF", "OECD")) {
        stop(sprintf("`pppex_src` must be either \"IMF\" or \"OECD\". '%s' is not allowed.", pppex_src))
      }

      # Validate input_data
      if (!is.data.frame(input_data)) {
        stop("`input_data` must be a data.frame or tibble.")
      }

      # Validate cost_base column
      if (!(cost_base %in% names(input_data))) {
        stop(sprintf("Column '%s' specified in `cost_base` not found in input_data.", cost_base))
      }

      # Validate cost_target name
      if (cost_target %in% names(input_data)) {
        warning(sprintf("Column '%s' already exists in input_data and will be overwritten.", cost_target))
      }

      # Load reference data
      tbl <- get_data(pppex_src, use_live_data, force_live_data)

      # Validate reference data structure
      required_cols <- c("country", "year", "value_gdpd", "value_pppex")
      if (!all(required_cols %in% names(tbl))) {
        stop("Reference data is missing required columns: country, year, value_gdpd, value_pppex.")
      }

      # Helper: create column if value is not a column name
      check_and_create_field <- function(df, field_name, user_value) {
        if (!(user_value %in% names(df))) {
          df[[field_name]] <- user_value
          user_value <- field_name
        }
        list(df, user_value)
      }

      # Apply helper to all relevant fields
      result <- check_and_create_field(input_data, "country_base", country_base)
      input_data <- result[[1]]
      country_base <- result[[2]]

      result <- check_and_create_field(input_data, "year_base", year_base)
      input_data <- result[[1]]
      year_base <- result[[2]]

      result <- check_and_create_field(input_data, "year_target", year_target)
      input_data <- result[[1]]
      year_target <- result[[2]]

      result <- check_and_create_field(input_data, "country_target", country_target)
      input_data <- result[[1]]
      country_target <- result[[2]]

      # Helper: ensure numeric conversion
      can_be_numeric <- function(x, field_name) {
        if (is.numeric(x)) {
          return(x)
        }
        if (all(grepl("^-?\\d*\\.?\\d+$", x))) {
          return(as.numeric(x))
        }
        stop(sprintf("Field '%s' must be numeric or contain only numeric characters.", field_name))
      }

      input_data[[cost_base]] <- can_be_numeric(input_data[[cost_base]], cost_base)
      input_data[[year_base]] <- can_be_numeric(input_data[[year_base]], year_base)
      input_data[[year_target]] <- can_be_numeric(input_data[[year_target]], year_target)

      # Clean country names if needed
      input_data$country_base2 <- if (rename_countries) country_cleaner(input_data[[country_base]]) else input_data[[country_base]]
      input_data$country_target2 <- if (rename_countries) country_cleaner(input_data[[country_target]]) else input_data[[country_target]]

      # Validate year-country combinations
      check_year_country_combination <- function(df, year_field, country_field, ref_tbl) {
        combos <- paste0(df[[country_field]], " - ", df[[year_field]])
        valid_combos <- paste0(ref_tbl$country, " - ", ref_tbl$year)
        invalid <- setdiff(combos, valid_combos)
        if (length(invalid) > 0) {
          stop(sprintf("Invalid year-country combinations found: %s", paste(invalid, collapse = ", ")))
        }
      }

      check_year_country_combination(input_data, year_base, "country_base2", tbl)
      check_year_country_combination(input_data, year_target, "country_target2", tbl)

      # Perform deflation
      output_data <- input_data |>
        dplyr::left_join(tbl, by = c("country_base2" = "country", year_base = "year")) |>
        dplyr::mutate(deflate_orig = as.numeric(value_gdpd), PPP_orig = as.numeric(value_pppex)) |>
        dplyr::select(-value_gdpd, -value_pppex) |>
        dplyr::left_join(tbl, by = c("country_target2" = "country", year_target = "year")) |>
        dplyr::mutate(deflate_target = as.numeric(value_gdpd), PPP_target = as.numeric(value_pppex)) |>
        dplyr::select(-value_gdpd, -value_pppex) |>
        dplyr::mutate(!!cost_target := (deflate_target * PPP_target) / (deflate_orig * PPP_orig) * .data[[cost_base]]) |>
        dplyr::select(-c("PPP_orig", "deflate_orig", "PPP_target", "deflate_target", "country_base2", "country_target2"))

      return(output_data)
    },
    error = function(e) {
      stop("deflate() failed: ", e$message)
    }
  )
}
