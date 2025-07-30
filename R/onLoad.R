.onLoad <- function(libname, pkgname) {
  tryCatch({
    prepare_temp_data()
  }, error = function(e) {
    warning("Failed to prepare temporary data on load: ", conditionMessage(e))
  })
}
