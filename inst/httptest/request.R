# httptest::set_requester(function(request) {
#   # Replace long base URL with a short alias
#   httptest::gsub_request(request, "https\\://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,1.0/A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...PPP_B1GQ.......?dimensionAtObservation=AllDimensions", "oecd/")
# })
#
# set_requester(function(request) {
#   # Replace long base URL with a short alias
#   gsub_request(request, "https\\://sdmx.oecd.org/public/rest/data/", "oecd/")
# })
httptest::set_requester(function(request) {
  # Replace the full OECD path with a short alias
  request |>
    httptest::gsub_request(
      # request,
      "https\\://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,1.0/A\\..*?PPP_B1GQ.*?dimensionAtObservation=AllDimensions",
      "oecd/ppp"
    ) |>
    # Shorten IMF API paths
    httptest::gsub_request(
      # request,
      "https\\://api.imf.org/external/sdmx/2.1/data/IMF.RES,WEO/\\*\\.NGDP_D\\.\\*",
      "imf/gdpd"
    ) |>
    httptest::gsub_request(
      # request,
      "https\\://api.imf.org/external/sdmx/2.1/data/IMF.RES,WEO/\\*\\.PPPEX\\.\\*",
      "imf/ppp"
    )
})
