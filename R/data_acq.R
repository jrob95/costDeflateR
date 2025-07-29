#----- OECD PPP
#' Get OECD PPP values
#'
#' @description
#' Get purchasing power parity values (PPP) from OECD data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant OECD PPP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a>https://gitlab.algobank.oecd.org/public-documentation/dotstat-migration/-/raw/main/OECD_Data_API_documentation.pdf</a> for more information
#'
#' @keyword Internal
#' @usage NULL

# Function to parse JSON from a URL and store in an environment variable
get_oecd_ppp <- function(url) {
  # Download JSON content
  response <- httr::GET(url)

  # Error checking - bad code?
  if (httr::http_status(response)$category != "Success") {
    stop("Failed to retrieve JSON. Check the URL.")
  }

  # Parse JSON content
  json_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Extract country codes and transaction types
  country_df <- json_data$structure$dimensions$observation$values[[which(json_data$structure$dimensions$observation$id == "REF_AREA")]]
  transaction_df <- json_data$structure$dimensions$observation$values[[which(json_data$structure$dimensions$observation$id == "TRANSACTION")]]
  years_df <- json_data$structure$dimensions$observation$values[[which(json_data$structure$dimensions$observation$id == "TIME_PERIOD")]]


  country_codes <- country_df$id
  # transactions - just incase we decide to use multiple measures at some point?
  transactions <- transaction_df$id
  years <- years_df$id


  # Extract observations from nested lists
  observations_df <- tibble::tibble(
    Key = names(json_data$dataSets$observations),
    Value = purrr::map_dbl(json_data$dataSets$observations, ~ .x[[1]][1])
  )

  # Split the key into components (assuming key structure aligns with order of metadata dimensions)
  observations_df2 <- observations_df |>
    tidyr::separate(Key, into = c("FREQ", "CountryIdx", "Sector", "CounterpartSector", "TransactionIdx"), sep = ":", extra = "drop") |>
    dplyr::mutate(
      CountryCode = country_codes[as.integer(CountryIdx) + 1],
      Transaction = transactions[as.integer(TransactionIdx) + 1]
    ) |>
    dplyr::group_by(CountryCode, Transaction) |>
    dplyr::mutate(Year = years[dplyr::row_number()]) |> # Assign unique years within each group
    dplyr::ungroup() |>
    dplyr::select(CountryCode, Year, Value) |>
    dplyr::arrange(CountryCode, Year)

  # Store in environment variable
  return(observations_df2)

  print("Parsing completed. Data stored in `parsed_data` environment variable.")
}




#----- IMF data
#' Get IMF data
#'
#' @description
#' Get purchasing power parity values (PPP) and values needed to calculate gross domestic product (GDP) from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF PPP? GDP deflator values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a>placeholderlink</a> for more information
#'
#' @keyword Internal
#' @usage NULL
get_imf <- function(query_url) {
  response <- httr::GET(query_url)

  if (httr::http_status(response)$category != "Success") {
    stop(paste0("Failed to fetch data from IMF API\n Query:", query_url, "returns:", httr::http_status(response)))
  }

  # Check if response contains only API metadata
  if (is.null(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$values)) {
    stop("Error: Query returned no data!")
  }


  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")) |>
    unlist() |>
    tibble::as_tibble(rownames = "names") |>
    tidyr::separate(names, into = c("type", "indicator", "country", "year"), sep = "\\.", fill = "right") |>
    dplyr::filter(type != "api") |>
    dplyr::rename(!!rlang::sym(stringr::str_extract(query_url, "[^/]+$")) := value) |> # Dynamic renaming
    dplyr::select(-type, -indicator)

  return(data)
}

#----- IMF PPP
#' Get IMF PPP values
#'
#' @description
#' Get purchasing power parity values (PPP) from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF PPP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a></a> for more information
#'
#' @keyword Internal
#' @usage NULL
get_imf_ppp <- function(){
  query_url <- "https://www.imf.org/external/datamapper/api/v1/PPPEX"
  get_imf(query_url)
}



#----- IMF gdp_r
#' Get IMF GDPR values
#'
#' @description
#' Get real gross domestic product values growth (GDP) from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF GDP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a></a> for more information
#'
#' @keyword Internal
#' @usage NULL

get_imf_gdpr <- function(){
  query_url <- "https://www.imf.org/external/datamapper/api/v1/NGDP_RPCH" # Real GDP Growth
  get_imf(query_url)
  }

#----- IMF gdp_n
#' Get IMF GDPR values
#'
#' @description
#' Get real gross domestic product values (GDP) from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF GDP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a></a> for more information
#'
#' @returns a `tibble` with 5 rows and variable number of observations (depending on year)
#' \describe{
#'  \item{country}{}
#'  \item{year}{}
#'  \item{gdp}{}}
#'
#' @source \href{https://www.imf.org/external/datamapper/api/v1/GDP}{API link}
#'
#' @keyword Internal
#' @usage NULL
get_imf_gdpn <- function(){
  query_url <- "https://www.imf.org/external/datamapper/api/v1/GDP" # Nominal GDP
  get_imf(query_url)
}

#----- IMF gdp_d
#' Get IMF GDP_D values
#'
#' @description
#' Calculate gross domestic product, deflator (GDP_D) values from IMF data sets
#'
#' @details
#' Internal function to be used by developers to update internal (i.e.: redundant IMF GDP values) and also by the user when loading the package to ensure up to date values are used in the `deflate()` function. see <a></a> for more information
#'
#' @keyword Internal
#' @usage NULL
#'
get_imf_gdpd <- function(){
  gdpn <- get_imf_gdpn() |>
    dplyr::full_join(get_imf_gdpr(), dplyr::join_by(country, year))
}
