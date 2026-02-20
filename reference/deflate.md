# Deflate currency values

\`deflate()\` returns a dataframe with deflated cost values, optionally
adding up to five new columns depending on the input structure.

## Usage

``` r
deflate(
  input_data,
  cost_base,
  year_base,
  country_base,
  year_target,
  country_target = "USA",
  cost_target = "cost_target",
  pppex_src = c("IMF", "OECD"),
  rename_countries = TRUE,
  use_live_data = TRUE,
  force_live_data = FALSE
)
```

## Arguments

- input_data:

  A \`data.frame\` containing the data to be deflated. Can be missing if
  values (or vectors of values) are specified for the cost, year and
  country base and targets.

- cost_base:

  A \`character\`: the name of the column in \`input_data\` containing
  base cost values.

- year_base:

  A \`numeric\` or \`character\`: either a year or the name of a column
  in \`input_data\` containing base years.

- country_base:

  A \`character\` string: either a country name or the name of a column
  in \`input_data\` containing base countries.

- year_target:

  A \`numeric\` or \`character\`: either a year or the name of a column
  in \`input_data\` containing target years.

- country_target:

  A \`character\`: either a country name or the name of a column in
  \`input_data\` containing target countries.

- cost_target:

  A \`character\`: the name for the new column that will contain
  deflated cost values. Defaults to \`"cost_target"\`.

- pppex_src:

  A \`character\`: the source of PPP data, either \`"IMF"\` or
  \`"OECD"\`.

- rename_countries:

  A \`logical\`: whether to standardize country names to ISO3C format.
  Defaults to \`TRUE\`.

- use_live_data:

  A \`logical\`: if \`TRUE\`, updates internal data if older than one
  week. Defaults to \`TRUE\`.

- force_live_data:

  A \`logical\`: if \`TRUE\`, forces an update of internal data
  regardless of age. Defaults to \`FALSE\`.

## Value

A \`data.frame\` with the original data and an additional column (named
by \`cost_target\`) containing deflated cost values.

## Details

This function deflates currency values using GDP deflators and PPP
exchange rates. You can either specify a base and target country/year
for each row via column names, or provide constant values to apply
across all rows.

## Examples

``` r
# Example 1: Using column names for base and target values
data <- data.frame(
  country_base = c("USA", "Germany"),
  year_base = c(2010, 2015),
  country_target = c("France", "Italy"),
  year_target = c(2015, 2020),
  cost_base = c(100, 200)
)

# Deflate currency values
deflated_data <- deflate(
  input_data = data,
  cost_base = "cost_base",
  year_base = "year_base",
  country_base = "country_base",
  year_target = "year_target",
  country_target = "country_target"
)
#> ℹ Attempting to use live data from IMF/ OECD
#> ℹ Updating IMF PPP data...
#> [rsdmx][INFO] Fetching 'https://api.imf.org/external/sdmx/2.1/data/IMF.RES,WEO/*.PPPEX.*/all/' 
#> ℹ Loaded live data: 
#> ✔ imf_ppp [6ms]
#> 
#> ℹ Updating IMF PPP data...
#> ✔ Updating IMF PPP data... [4.2s]
#> 
#> ℹ Attempting to use live data from IMF/ OECD
#> ℹ Updating IMF GDPD data...
#> [rsdmx][INFO] Fetching 'https://api.imf.org/external/sdmx/2.1/data/IMF.RES,WEO/*.NGDP_D.*/all/' 
#> ℹ Loaded live data: 
#> ✔ imf_gdpd [7ms]
#> 
#> ℹ Updating IMF GDPD data...
#> ✔ Updating IMF GDPD data... [3.8s]
#> 
#> ℹ Attempting to use live data from IMF/ OECD
#> ✔ Attempting to use live data from IMF/ OECD [8.1s]
#> 

# You can also also specify values in the function for year and country
# Example 2: Using constant values for base and target
deflated_data <- deflate(
  cost_base = c(100, 200),
  year_base = "2010",
  country_base = "Australia",
  year_target = "2020",
  country_target = "United States",
  rename_countries = TRUE
)
#> ℹ Attempting to use live data from IMF/ OECD
#> ✔ Attempting to use live data from IMF/ OECD [16ms]
#> 
```
