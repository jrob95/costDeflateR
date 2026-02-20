can_be_numeric <- function(x, field_name) {
  if (is.numeric(x)) {
    return(x)
  }
  if (all(grepl("^-?\\d*\\.?\\d+$", x))) {
    return(as.numeric(x))
  }
  cli::cli_abort(
    "Field '{field_name}' must be numeric or contain only numeric characters."
  )
}


check_year_country_combination <- function(
  df,
  year_field,
  country_field,
  ref_tbl
) {
  # Validate year-country combinations
  combos <- paste0(df[[country_field]], " - ", df[[year_field]])
  valid_combos <- paste0(ref_tbl$country, " - ", ref_tbl$year)
  invalid <- setdiff(combos, valid_combos)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Invalid year-country combinations found: {paste(invalid, collapse = ', ')}"
    )
  }
}


is_internet_down <- function() {
  !curl::has_internet()
}


check_internet <- function() {
  if (is_internet_down()) {
    cli::cli_abort("Check your internet connection")
  }
}
