
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
         Transaction = transactions[as.integer(TransactionIdx) + 1]) |>
       dplyr::group_by(CountryCode, Transaction) |>
       dplyr::mutate(Year = years[dplyr::row_number()]) |>  # Assign unique years within each group
       dplyr::ungroup() |>
       dplyr::select(CountryCode, Year, Value) |>
       dplyr::arrange(CountryCode, Year)

     # Store in environment variable
     return(observations_df2)

     print("Parsing completed. Data stored in `parsed_data` environment variable.")
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

   flowRef <- "IMF.RES"
   key <- "PPP_EX.A.ALL"
   providerRef <- "IMF"
   query_url <- paste0(
     "https://api.imf.org/external/sdmx/2.1/availableconstraint/",
     flowRef, "/", key, "/", providerRef,
     "?startPeriod=2000&endPeriod=2025"
   )


   response <- httr::GET(query_url)

   if (httr::http_status(response)$category != "Success") {
     stop("Failed to fetch PPP_EX data from IMF API")
   }

   data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

   # Inspect structure
   str(data)



