# Conditionally update internal data

Checks the age of each dataset in the temporary directory and updates
only those that are missing or older than 7 days, or if \`force\` is set
to TRUE.

## Usage

``` r
cond_update_internal_data(
  force = FALSE,
  dl_oecdppp = TRUE,
  dl_imfppp = TRUE,
  dl_imfgdpd = TRUE
)
```

## Arguments

- force:

  Logical. If TRUE, forces the update regardless of data age.

- dl_oecdppp:

  Logical. Whether to check/update OECD PPP data.

- dl_imfppp:

  Logical. Whether to check/update IMF PPP data.

- dl_imfgdpd:

  Logical. Whether to check/update IMF GDP deflator data.

## Value

Path to the temporary data directory
