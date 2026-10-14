# Run the Clusnalyst app

Run the Clusnalyst app

## Usage

``` r
run_app(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
)
```

## Arguments

- onStart, options, enableBookmarking, uiPattern:

  Passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- ...:

  Options passed to the app as golem options.

## Value

A Shiny app object.

## Examples

``` r
if (interactive()) run_app()
```
