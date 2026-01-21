# IMF/ OECD data country/ year combo list

\`delfator_country_year_combs\` returns a returns a list of available
countriy/ year combinations in the WEO and/ or OECD PPPEX and defaltor
data sets.

## Usage

``` r
delfator_country_year_combs(
  pppex_src = "IMF",
  use_live_data = TRUE,
  force_live_data = FALSE
)
```

## Arguments

- pppex_src:

  A \`character\`, which dataset should PPP values come from? IMF or
  OECD?

- use_live_data:

  A \`logical\`. Makes call to \`update_internal_data()\`, if current
  IMF or OECD are more than a week old then make an API call to replace
  them., default = \`TRUE\`

- force_live_data:

  A \`logical\`. Makes call to \`update_internal_data()\` regardless of
  age of data currently stored. Use only if you know IMF WEO or OECD
  data has been updated since you last ran

## Value

Returns a `tibble`.

## Details

This is a helper function for checking which country/ year combinations
exist in the reference data
