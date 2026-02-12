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
#' @param input_data A `data.frame` containing the data to be deflated. Can be missing if values (or vectors of values) are specified for the cost, year and country base and targets.
#' @param cost_base A `character`: the name of the column in `input_data` containing base cost values.
#' @param year_base A `numeric` or `character`: either a year or the name of a column in `input_data` containing base years.
#' @param country_base A `character` string: either a country name or the name of a column in `input_data` containing base countries.
#' @param year_target A `numeric` or `character`: either a year or the name of a column in `input_data` containing target years.
#' @param country_target A `character`: either a country name or the name of a column in `input_data` containing target countries.
#' @param cost_target A `character`: the name for the new column that will contain deflated cost values. Defaults to `"cost_target"`.
#' @param pppex_src A `character`: the source of PPP data, either `"IMF"` or `"OECD"`.
#' @param rename_countries A `logical`: whether to standardize country names to ISO3C format. Defaults to `TRUE`.
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
#' # Example 2: Using constant values for base and target
#' deflated_data <- deflate(
#'   cost_base = c(100, 200),
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
  pppex_src = c("IMF", "OECD"),
  rename_countries = TRUE,
  use_live_data = TRUE,
  force_live_data = FALSE
) {
  pppex_src <- match.arg(pppex_src)

  if (!missing(input_data)) {
    checkmate::assert_class(input_data, "data.frame")
  } else {
    # data are specified with vectors for cost_base -> year_target
    # combine them into a dataframe so that they can be validated using the same approach
    # first have to check that they are the right length and class

    assertthat::assert_that(length(cost_base) > 0)
    valid_input_length <- c(1, length(cost_base))

    assertthat::assert_that(length(year_base) %in% valid_input_length)
    assertthat::assert_that(length(country_base) %in% valid_input_length)
    assertthat::assert_that(length(year_target) %in% valid_input_length)
    assertthat::assert_that(length(country_target) %in% valid_input_length)

    input_data <- data.frame(
      cost_base = cost_base,
      year_base = year_base,
      country_base = country_base,
      year_target = year_target,
      country_target = country_target
    )

    cost_base <- "cost_base"
    year_base <- "year_base"
    country_base <- "country_base"
    year_target <- "year_target"
    country_target <- "country_target"
  }

  # input_data has been created at this point even if not given as arg
  # check that columns exist in input_data as they ought to.
  checkmate::assert_scalar(cost_base)
  checkmate::assert_character(cost_base)

  checkmate::assert_scalar(year_base)
  checkmate::assert_character(year_base)

  checkmate::assert_scalar(country_base)
  checkmate::assert_character(country_base)

  checkmate::assert_scalar(year_target)
  checkmate::assert_character(year_target)

  checkmate::assert_names(
    names(input_data),
    must.include = c(cost_base, year_base, country_base, year_target)
  )

  # need to be able to add these as columns to the dataset (after checking that they aren't already columns - throw error if so)
  # and then continue with the updated arg value being the new column name which contains the new column as that one input value

  checkmate::assert_scalar(cost_target)

  if (cost_target %in% names(input_data)) {
    warning(sprintf(
      "Column '%s' already exists in input_data and will be overwritten.",
      cost_target
    ))
    input_data <- dplyr::select(input_data, -!!rlang::sym(cost_target))
  }

  tbl <- get_data(pppex_src, use_live_data, force_live_data)

  # Validate reference data structure
  checkmate::assert_names(
    names(tbl),
    must.include = c("country", "year", "value_gdpd", "value_pppex")
  )

  input_data[[cost_base]] <- can_be_numeric(
    input_data[[cost_base]],
    cost_base
  )

  input_data[[year_base]] <- can_be_numeric(
    input_data[[year_base]],
    year_base
  )
  input_data[[year_target]] <- can_be_numeric(
    input_data[[year_target]],
    year_target
  )

  # Clean country names if needed
  if (rename_countries) {
    input_data$country_base2 <- country_cleaner(input_data[[country_base]])
    input_data$country_target2 <- country_cleaner(input_data[[country_target]])
  } else {
    input_data$country_base2 <- input_data[[country_base]]
    input_data$country_target2 <- input_data[[country_target]]
  }

  check_year_country_combination(
    input_data,
    year_base,
    "country_base2",
    tbl
  )

  check_year_country_combination(
    input_data,
    year_target,
    "country_target2",
    tbl
  )

  # Perform deflation
  input_data |>
    dplyr::left_join(
      tbl,
      dplyr::join_by(
        !!rlang::sym("country_base2") == country,
        !!rlang::sym(year_base) == year
      )
    ) |>
    dplyr::mutate(
      deflate_orig = as.numeric(value_gdpd),
      PPP_orig = as.numeric(value_pppex)
    ) |>
    dplyr::select(-value_gdpd, -value_pppex) |>
    dplyr::left_join(
      tbl,
      dplyr::join_by(
        !!rlang::sym("country_target2") == country,
        !!rlang::sym(year_target) == year
      )
    ) |>
    dplyr::mutate(
      deflate_target = as.numeric(value_gdpd),
      PPP_target = as.numeric(value_pppex)
    ) |>
    dplyr::select(-value_gdpd, -value_pppex) |>
    dplyr::mutate(
      !!cost_target := (deflate_target * PPP_target) /
        (deflate_orig * PPP_orig) *
        .data[[cost_base]]
    ) |>
    dplyr::select(
      -c(
        "PPP_orig",
        "deflate_orig",
        "PPP_target",
        "deflate_target",
        "country_base2",
        "country_target2"
      )
    )
}


# Helper: create column if value is not a column name
check_and_create_field <- function(df, field_name, user_value) {
  if (!(user_value %in% names(df))) {
    df[[field_name]] <- user_value
    user_value <- field_name
  }
  list(df, user_value)
}
