# Get data

\`get_data\` Checks user preference for source of OECD/ IMF PPPEX and
GDPD data (internal or live) and loads data accordingly.

## Usage

``` r
get_data(pppex_src, use_live_data, force_live_data)
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
