#' Deflate currency values.
#' @title
#' Deflate currency values
#'
#' @description
#' `deflate2` returns a dataframe with up to 5 additional columns, and main calculates deflated cost values
#'
#' @details
#' This is a function for deflating currency, specifying either a base and target country/
#' year on each row, or applying a pre specified country/ year for each row.
#'
#' @param input_data A `data.frame`.
#' @param country_base A `character` the name of a country OR the `colname` representing the base country.
#' @param year_base A `numeric` or `colname` representing the base year.
#' @param year_target A `numeric` or `colname` representing the target year.
#' @param country_target A `character` or `colname` representing the target country.
#' @param cost_base A `numeric` or `colname` representing the base cost.
#' @param cost_target A `character`, what should the name be for the new column with deflated cost values.
#' @param rename_countries A `logical`, ask the function to clean up country names if not in `iso3c` format.
#' @param pppex_src A `character`, which dataset should PPP values come from? IMF or OECD?
#' @param use_live_data A `logical`. Makes call to `update_internal_data()`, if current IMF or OECD are more than a week old then make an API call to replace them., default = `TRUE`
#' @param force_live_data A `logical`. Makes call to `update_internal_data()` regardless of age of data currently stored. Use only if you know IMF WEO or OECD data has been updated since you last ran `deflate`, default = `FALSE`
#'  Default to \code{"cost_target"}
#'
#' @return Returns a dataframe with deflated cost values.
#'
#' @examples
#' # Sample dataframe
#' data <- data.frame(
#'   country_base = c("USA", "Germany"),
#'   year_base = c(2010, 2015),
#'   country_target = c("France", "Italy"),
#'   year_target = c(2015, 2020),
#'   cost_base = c(100, 200)
#' )
#'
#' # Deflate currency values
#' deflated_data <- deflate2(
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
#' # Deflate currency values with pre specified values
#' deflated_data <- deflate2(
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
deflate2 <- function(
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
  # tryCatch(
  #   {
  #     if (!pppex_src %in% c("IMF", "OECD")) {
  #       stop(sprintf("`pppex_src` must be either \"IMF\" or \"OECD\". '%s' is not allowed. Check capitalization.", pppex_src))
  #     }
  #
  #     tbl <- get_data(pppex_src, use_live_data, force_live_data)
  #     # check parameters####
  #     # handler to throw errors if desired object is not class
  #     if (!is.data.frame(input_data) & !tibble::is_tibble(input_data)) {
  #       stop("Data must be in the format of a `data.frame` or `tibble`")
  #     }
  #
  #     # Function to check if a field exists in the dataframe and create it if it doesn't
  #     check_and_create_field <- function(field_name, user_value, x = input_data) {
  #       if (!(user_value %in% names(x))) {
  #         x[, field_name] <- user_value
  #         # overwrite user_value with the value of the field in updated table.
  #         user_value <- field_name
  #       }
  #       return(list(x, user_value)) # Return both the updated dataframe and user_value
  #     }
  #
  #     # Function to handle result and make it repeatable
  #     handle_result <- function(result) {
  #       input_data <<- result[[1]] # Update global input_data
  #       return(result[[2]]) # Return updated user_value
  #     }
  #
  #     country_base <- handle_result(check_and_create_field("country_base", country_base))
  #     year_base <- handle_result(check_and_create_field("year_base", year_base))
  #     year_target <- handle_result(check_and_create_field("year_target", year_target))
  #     country_target <- handle_result(check_and_create_field("country_target", country_target))
  #
  #
  #     # Function to check if fields are numeric or can be numeric
  #
  #     can_be_numeric <- function(x, field_name = "unknown") {
  #       if (is.numeric(x)) {
  #         return(x)
  #       }
  #       if (all(grepl("^-?\\d*\\.?\\d+$", x))) {
  #         return(as.numeric(x))
  #       }
  #       stop(sprintf("Field '%s' must be numeric or contain only numeric characters.", field_name))
  #     }
  #
  #
  #     input_data[, cost_base] <- can_be_numeric(input_data[[cost_base]], cost_base)
  #     input_data[, year_base] <- can_be_numeric(input_data[[year_base]], cost_base)
  #     input_data[, year_target] <- can_be_numeric(input_data[[year_target]], cost_base)
  #
  #
  #     # Check if country_base is included in gdpd_vals or is a column in data frame
  #     if (rename_countries) {
  #       input_data[["country_base2"]] <- country_cleaner(input_data[[country_base]])
  #       input_data[["country_target2"]] <- country_cleaner(input_data[[country_target]])
  #     } else {
  #       input_data[["country_base2"]] <- input_data[[country_base]]
  #       input_data[["country_target2"]] <- input_data[[country_target]]
  #     }
  #
  #     # # Check if the combination of year and country is available in the data
  #     check_year_country_combination <- function(x, year_field, country_field, tbl) {
  #       # get number of unique combinations'
  #       combs <- unique(paste0(x[[country_field]], " - ", as.character(x[[year_field]])))
  #       ref_comb <- paste0(tbl$country, " - ", as.character(tbl$year))
  #       # check whether the combinations are in the reference table
  #       for (comb in combs) {
  #         if (!(comb %in% ref_comb)) {
  #           # stop(paste0("\"", comb, "\" does not exist in the reference table. Please use `deflator_country_year_combs()` to check available combinations"))
  #           stop(paste0("\"", comb, "\" does not exist in the reference table. Please check available combinations"))
  #         }
  #       }
  #     }
  #
  #     check_year_country_combination(input_data, year_base, "country_base2", tbl)
  #     check_year_country_combination(input_data, year_target, "country_target2", tbl)
  #
  #     # deflate the cleaned input data
  #     output_data <- input_data |>
  #       dplyr::left_join(tbl, dplyr::join_by("country_base2" == "country", !!year_base == "year")) |>
  #       dplyr::mutate(
  #         deflate_orig = as.numeric(value_gdpd),
  #         PPP_orig = as.numeric(value_pppex)
  #       ) |>
  #       dplyr::select(-value_gdpd, -value_pppex) |>
  #       dplyr::left_join(tbl, dplyr::join_by("country_target2" == "country", !!year_target == "year")) |>
  #       dplyr::mutate(
  #         deflate_target = as.numeric(value_gdpd),
  #         PPP_target = as.numeric(value_pppex)
  #       ) |>
  #       dplyr::select(-value_gdpd, -value_pppex) |>
  #       dplyr::mutate(cost_target = (deflate_target * PPP_target) / (deflate_orig * PPP_orig) * .data[[cost_base]]) |>
  #       dplyr::select(-c("PPP_orig", "deflate_orig", "PPP_target", "deflate_target", "country_base2", "country_target2"))
  #     return(output_data)
  #   },
  #   error = function(e) {
  #     stop("deflate() failed: ", e$message)
  #   }
  # )
}


