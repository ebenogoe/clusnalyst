# Save and restore an analysis session

A session file is JSON: the data, the metadata and the settings. Results
are recomputed when the session is opened again, so nothing executable
is stored.

## Usage

``` r
save_session(d, settings, path)

load_session(path)
```

## Arguments

- d:

  The `cl_data` as read.

- settings:

  The settings list.

- path:

  File to write or read.

## Value

`save_session()` returns `path` invisibly; `load_session()` returns a
list with `data` and `settings`.
