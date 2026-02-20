# Prepare internal data in temporary directory

Downloads OECD and IMF datasets and stores them in a temporary
directory. Falls back to internal data if download fails.

## Usage

``` r
safe_fetch(fetch_fn, fallback_name, filename, dir, force)
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

- force:

  Logical. If TRUE, forces the update regardless of data age.

## Value

Path to the temporary data directory
