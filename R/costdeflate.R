#' Deflate currency values.
#' @title
#' Deflate currency values
#'
#' @description
#' `deflate` returns a dataframe with up to 5 additional columns, and main calculates deflated cost values
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
#' # Deflate currency values with pre specified values
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
deflate <- function(input_data = .x, cost_base, year_base, country_base, year_target, country_target, cost_target = "cost_target", pppex_src = "IMF", rename_countries = FALSE, use_live_data = TRUE, force_live_data = FALSE) {
  tryCatch(
    {
      if (!pppex_src %in% c("IMF", "OECD")) {
        stop(paste0("`pppex_src` must be either \"IMF\" or \"OECD\"\"", pppex_src, "\" not allowed. Check capitals/ case."))
      }

      # deal with internal/ live data.
      dl_imfppp <- dl_oecdppp <- FALSE

      if (pppex_src == "IMF") dl_imfppp <- TRUE
      if (pppex_src == "OECD") dl_oecdppp <- TRUE
      # get live data, store temp.
      if (use_live_data) {
        message("Attempting to use live data from IMF/ OECD")
        cond_update_internal_data(force_live_data, dl_imfppp, dl_oecdppp)
        dir <- get_temp_data_dir()
        oecd_ppp <- readRDS(file.path(dir, "oecd_ppp.rds"))
        imf_ppp <- readRDS(file.path(dir, "imf_ppp.rds"))
        imf_gdpd <- readRDS(file.path(dir, "imf_gdpd.rds"))
      } else {
        message(paste0("Using internal data. Last upated", update_meta[updated_at]))
      }

      # point data to correct dataset.
      if (pppex_src == "IMF") {
        ppp_vals <- imf_ppp |>
          dplyr::select(country = COUNTRY, year = TIME_PERIOD, value = PPPEX)
      }
      if (pppex_src == "OECD") {
        ppp_vals <- oecd_ppp |>
          dplyr::select(country = COUNTRY, year = TIME_PERIOD, value = PPP)
      }

      if (TRUE) {
        gdpd_vals <- imf_gdpd |>
          dplyr::select(country = COUNTRY, year = TIME_PERIOD, value = NGDP_D)
      }

      # country combs for later
      tbl <- ppp_vals |>
        dplyr::inner_join(gdpd_vals, dplyr::join_by(country, year), suffix = c("_pppex", "_gdpd"))

      # check parameters####
      # handler to throw errors if desired object is not class
      if (!is.data.frame(input_data) & !tibble::is_tibble(input_data)) {
        stop("Data must be in the format of a `data.frame` or `tibble`")
      }

      # Function to check if a field exists in the dataframe and create it if it doesn't
      check_and_create_field <- function(field_name, user_value, x = input_data) {
        if (!(user_value %in% names(x))) {
          x[, field_name] <- user_value
          # overwrite user_value with the value of the field in updated table.
          user_value <- field_name
        }
        return(list(x, user_value)) # Return both the updated dataframe and user_value
      }

      # Function to handle result and make it repeatable
      handle_result <- function(result) {
        input_data <<- result[[1]] # Update global input_data
        return(result[[2]]) # Return updated user_value
      }

      country_base <- handle_result(check_and_create_field("country_base", country_base))
      year_base <- handle_result(check_and_create_field("year_base", year_base))
      year_target <- handle_result(check_and_create_field("year_target", year_target))
      country_target <- handle_result(check_and_create_field("country_target", country_target))


      # Function to check if fields are numeric or can be numeric
      can_be_numeric <- function(x) {
        if (is.numeric(x)) {
          return(x) # If x is already numeric, return it directly
        }

        all_numbers <- all(grepl("^-?\\d*\\.?\\d+$", x)) # Check if all elements match a numeric pattern
        if (all_numbers) {
          num <- as.numeric(x) # Convert to numeric
          return(num)
        } else {
          stop(sprintf("Field '%s' must be numeric or contain only numeric characters.", field_name))
        }
      }

      input_data[, cost_base] <- can_be_numeric(input_data[[cost_base]])
      input_data[, year_base] <- can_be_numeric(input_data[[year_base]])
      input_data[, year_target] <- can_be_numeric(input_data[[year_target]])


      # Check if country_base is included in gdpd_vals or is a column in data frame
      if (rename_countries) {
        input_data[["country_base2"]] <- country_cleaner(input_data[[country_base]])
        input_data[["country_target2"]] <- country_cleaner(input_data[[country_target]])
      } else {
        input_data[["country_base2"]] <- input_data[[country_base]]
        input_data[["country_target2"]] <- input_data[[country_target]]
      }

      # # Check if the combination of year and country is available in the data
      check_year_country_combination <- function(x, year_field, country_field, tbl) {
        # get number of unique combinations'
        combs <- unique(paste0(x[[country_field]], " - ", as.character(x[[year_field]])))
        ref_comb <- paste0(tbl$country, " - ", as.character(tbl$year))
        # check whether the combinations are in the reference table
        for (comb in combs) {
          if (!(comb %in% ref_comb)) {
            # stop(paste0("\"", comb, "\" does not exist in the reference table. Please use `delfator_country_year_combs()` to check available combinations"))
            stop(paste0("\"", comb, "\" does not exist in the reference table. Please check available combinations"))
          }
        }
      }

      check_year_country_combination(input_data, year_base, "country_base2", tbl)
      check_year_country_combination(input_data, year_target, "country_target2", tbl)

      # deflate the cleaned input data
      output_data <- input_data %>%
        dplyr::left_join(tbl, dplyr::join_by("country_base2" == "country", !!year_base == "year")) %>%
        dplyr::mutate(
          deflate_orig = as.numeric(value_gdpd),
          PPP_orig = as.numeric(value_pppex)
        ) %>%
        dplyr::select(-value_gdpd, -value_pppex) %>%
        dplyr::left_join(tbl, dplyr::join_by("country_target2" == "country", !!year_target == "year")) %>%
        dplyr::mutate(
          deflate_target = as.numeric(value_gdpd),
          PPP_target = as.numeric(value_pppex)
        ) %>%
        dplyr::select(-value_gdpd, -value_pppex) %>%
        dplyr::mutate(cost_target = (deflate_target * PPP_target) / (deflate_orig * PPP_orig) * .data[[cost_base]]) %>%
        dplyr::select(-c("PPP_orig", "deflate_orig", "PPP_target", "deflate_target", "country_base2", "country_target2"))
      return(output_data)
    },
    error = function(e) {
      message("Error: ", e$message)
    }
  )
}

#' Deflator country list
#'
#' `deflator_country_list` returns a returns a list of available countries in the WEO data set.
#'
#' This is a helper function for checking which countries exist in the reference data
#'
#' @return Returns a \code{chr}.
#' @export
deflator_country_list <- function() {
  return(unique(gdpd_vals$Country))
}

#' Deflate country/ year combo list
#'
#' `delfator_country_year_combs` returns a returns a list of available countriy/ year combinations in the WEO data set.
#'
#' This is a helper function for checking which countr/ year combinations  exist in the reference data
#'
#' @return Returns a \code{chr}.
delfator_country_year_combs <- function() {
  return(paste0(gdpd_vals$Country, " - ", as.character(gdpd_vals$year)))
}

use_live_data <- function() {
  tryCatch(
    {
      update_internal_data()
      packageStartupMessage(
        "Using latest IMF & OECD datasets - updated ",
        Sys.time()
      )
    },
    error = function(e) {
      last <- tryCatch(
        get("update_meta",
          envir = asNamespace("costDeflateR")
        )[["updated_at"]],
        error = function(e) "unknown"
      )
      warning(
        "Failed to prepare temporary data on load: ",
        conditionMessage(e),
        "\n Reverting to internal data. Last updated: ",
        last
      )
    }
  )
}

country_cleaner <- function(data) {
  countrycode::countryname(data, destination = "iso3c")
}
