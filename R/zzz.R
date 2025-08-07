# .onLoad <- function(lib, pkg) {
#   rlang::run_on_load()
# }
#
# rlang::on_load({
#   tryCatch(
#     {
#       update_internal_data()
#       packageStartupMessage("Using latest IMF & OECD datasets - updated ",
#                             Sys.time())
#     },
#     error = function(e) {
#       last <- tryCatch(get("update_meta",
#                            envir = asNamespace("costDeflateR"))[["updated_at"]],
#                        error = function(e) "unknown"
#       )
#       warning(
#         "Failed to prepare temporary data on load: ",
#         conditionMessage(e),
#         "\n Reverting to internal data. Last updated: ",
#         last
#       )
#     }
#   )
# })
#

utils::globalVariables(c(
  ".data", ".x", "updated_at", "COUNTRY", "TIME_PERIOD", "PPPEX", "PPP", "NGDP_D",
  "country", "year", "field_name", "value_gdpd", "value_pppex",
  "deflate_target", "PPP_target", "deflate_orig", "PPP_orig", "gdpd_vals",
  "update_internal_data", "country.name.en", "iso3c", "OBS_VALUE", "REF_AREA", "obsValue",
  ":="
))
