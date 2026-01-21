# Prepare internal data in temporary directory

Downloads OECD and IMF datasets and stores them in a temporary
directory. Falls back to internal data if download fails.

## Usage

``` r
safe_fetch(fetch_fn, fallback_name, filename, dir)
```

## Arguments

- fetch_fn:

  function to use to fetch data from api.

- fallback_name:

  name of internal dataset to use as fallback.

- filename:

  name of file in temp folder.

- dir:

  character string contain temp directory path.

## Value

Path to the temporary data directory
