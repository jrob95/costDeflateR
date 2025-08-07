# Script to generate mock HTTP failure response files for testthat/httptest tests

# Create directory if it doesn't exist
mock_dir <- "tests/testthat/api"
if (!dir.exists(mock_dir)) {
  dir.create(mock_dir, recursive = TRUE)
}

# Define functions and error types
functions <- c("get_oecd_ppp", "get_imf", "get_imf_gdpd", "get_imf_ppp")
errors <- list(
  "404" = "HTTP/1.1 404 Not Found\n\n{\"error\": \"Not Found\"}",
  "429" = "HTTP/1.1 429 Too Many Requests\n\n{\"error\": \"Rate limit exceeded\"}",
  "connection" = "HTTP/1.1 000 Connection Error\n\n{\"error\": \"Could not resolve host\"}"
)

# Generate mock files
for (func in functions) {
  for (code in names(errors)) {
    content <- errors[[code]]
    file_path <- file.path(mock_dir, paste0(func, "_", code, ".mock"))
    writeLines(content, file_path)
  }
}

message("✅ Mock failure files have been generated in 'tests/testthat/api'.")
