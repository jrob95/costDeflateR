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
#' @param country_base A `character` the name of a country OR the column name representing the base country.
#' @param year_base A numeric or column name representing the base year.
#' @param year_target A numeric or column name representing the target year.
#' @param country_target A character or column name representing the target country.
#' @param cost_base A numeric or column name representing the base cost.
#' @param cost_target String, what should the name be for the new column with deflated cost values.
#' @param rename_countries A Logical
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
#'   country_target = "United States"
#' )
#'
#' @export
deflate <- function(input_data = .x, cost_base, year_base, country_base, year_target, country_target, cost_target = "cost_target", rename_countries = FALSE, use_live_data = TRUE) {

  # get live data, store temp.
  if (use_live_data == TRUE) {
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

  tryCatch(
    {
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
          stop("All elements must be numeric, or characters with only numbers")
        }
      }

      input_data[, cost_base] <- can_be_numeric(input_data[[cost_base]])
      input_data[, year_base] <- can_be_numeric(input_data[[year_base]])
      input_data[, year_target] <- can_be_numeric(input_data[[year_target]])


      # Check if country_base is included in gdpd_vals or is a column in data frame
      # Fuzzy match with agrep if name is close
      check_countries <- function(country_vector) {
        if (!is.character(country_vector)) {
          stop("Country vector must be a character")
        }

        # Convert country_vector to title case and create a factor
        country_vector <- tools::toTitleCase(country_vector)

        # Get unique values from country_vector
        unique_countries <- unique(country_vector)
        unique_in_reference <- unique(tools::toTitleCase(gdpd_vals$Country))

        # Initialize an empty vector to store replacements
        replacements <- country_vector

        # Initialize empty vectors to store indices of matched countries
        matched_indices_country_vector <- integer(0)
        matched_indices_gdpd_vals <- integer(0)

        for (country in unique_countries) {
          # Skip the value if it exactly matches any value in unique_in_reference
          if (country %in% unique_in_reference) {
            next # Skip to the next country
          }

          # Fuzzy match country with unique_in_reference
          matched_index <- agrep(country, unique_in_reference, max.distance = 0.1) # Adjust max.distance as needed

          # If a match is found, replace and print the replacement
          if (length(matched_index) > 0) {
            replacement <- unique_in_reference[matched_index[1]] # Take the first match
            matched_indices_country_vector <- c(matched_indices_country_vector, which(country_vector == country)) # Add indices of matched countries in country_vector
            matched_indices_gdpd_vals <- c(matched_indices_gdpd_vals, matched_index) # Add indices of matched countries in unique_in_reference
            cat("Country checker function replaced", country, "with", replacement, "\n")
          }
        }

        # Replace values in country_vector with replacements for matched indices
        replacements[matched_indices_country_vector] <- unique_in_reference[matched_indices_gdpd_vals]

        # # Update country_vector with replacements
        country_vector <- replacements

        # check that is a good match
        countries_in_gdpd <- country_vector %in% gdpd_vals$Country
        if (!all(countries_in_gdpd)) {
          invalid_countries <- country_vector[!countries_in_gdpd]
          stop(writeLines(paste0("\`", invalid_countries, "\` or a similar name is not included in the WEO reference table. Use deflator_country_list() for a list of acceptable countries/ country names")))
        }

        return(country_vector)
      }


      input_data[[country_base]] <- check_countries(input_data[[country_base]])
      input_data[[country_target]] <- check_countries(input_data[[country_target]])


      # # Check if the combination of year and country is available in the data
      check_year_country_combination <- function(x, year_field, country_field) {
        # get number of unique combinations'
        combs <- unique(paste0(x[[country_field]], " - ", as.character(x[[year_field]])))
        ref_comb <- paste0(gdpd_vals$Country, " - ", as.character(gdpd_vals$year))
        # check whether the combinations are in the reference table
        for (comb in combs) {
          if (!(comb %in% ref_comb)) {
            stop(paste0("\"", comb, "\" does not exist in the reference table. Please use delfator_country_year_combs() to check available combinations"))
          }
        }
      }

      check_year_country_combination(input_data, year_base, country_base)
      check_year_country_combination(input_data, year_target, country_target)

      # deflate the cleaned input data
      output_data <- input_data %>%
        dplyr::left_join(gdpd_vals, dplyr::join_by(!!country_base == "Country", !!year_base == "year")) %>%
        dplyr::mutate(deflate_orig = as.numeric(value)) %>%
        dplyr::select(-value) %>%
        dplyr::left_join(gdpd_vals, dplyr::join_by(!!country_target == "Country", !!year_target == "year")) %>%
        dplyr::mutate(deflate_target = as.numeric(value)) %>%
        dplyr::select(-value) %>%
        dplyr::left_join(ppp_vals, dplyr::join_by(!!country_base == "Country", !!year_base == "year")) %>%
        dplyr::mutate(PPP_orig = as.numeric(value)) %>%
        dplyr::select(-value) %>%
        dplyr::left_join(ppp_vals, dplyr::join_by(!!country_target == "Country", !!year_target == "year")) %>%
        dplyr::mutate(PPP_target = as.numeric(value)) %>%
        dplyr::select(-value) %>%
        dplyr::mutate(cost_target = (deflate_target * PPP_target) / (deflate_orig * PPP_orig) * .data[[cost_base]]) %>%
        dplyr::select(-c("PPP_orig", "deflate_orig", "PPP_target", "deflate_target"))
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
#' @export
delfator_country_year_combs <- function() {
  return(paste0(gdpd_vals$Country, " - ", as.character(gdpd_vals$year)))
}
