# Default analysis settings

Every step of the app is described by one entry in this list. The app
fills it in as you go,
[`run_analysis()`](https://ebenogoe.github.io/clusnalyst/reference/run_analysis.md)
replays it, and
[`build_script()`](https://ebenogoe.github.io/clusnalyst/reference/build_script.md)
turns it into R code.

## Usage

``` r
default_settings(d = NULL)
```

## Arguments

- d:

  Optional `cl_data`; picks defaults that suit its type.

## Value

A named list.
