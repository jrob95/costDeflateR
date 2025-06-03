# Script for development team to update internal (redundancy) data sets.



############
# OECD PPP #
############


oecd_url_stub <- "https://sdmx.oecd.org/public/rest/"
oecd_dataset <- "data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,2.0/"
oecd_filter <- "A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...PPP_B1GQ.......?dimensionAtObservation=AllDimensions"
# as far as I can tell, "Purchasing power parities for GDP" (PPP_B1GQ) were used, not "Purchasing Power Parities for actual individual consumption" (PPP_P41)
# see https://gitlab.algobank.oecd.org/public-documentation/dotstat-migration/-/raw/main/OECD_Data_API_documentation.pdf for more information

oecd_url <- paste0(oecd_url_stub,oecd_dataset, oecd_filter)

### Developer -> update internal data sets
oecd <- parse_oecd_json(oecd_url)
usethis::use_data(oecd_ppp, overwrite = TRUE)
