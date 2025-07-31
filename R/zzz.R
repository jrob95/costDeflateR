.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}

rlang::on_load({
  tryCatch(
    {
      update_internal_data()
      packageStartupMessage("Using latest IMF & OECD datasets - updated ",
                            Sys.time())
    },
    error = function(e) {
      last <- tryCatch(get("update_meta",
                           envir = asNamespace("costDeflateR"))[["updated_at"]],
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
})
