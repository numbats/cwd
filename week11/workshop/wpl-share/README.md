# wplshare

Starter repository for ETC5523 Workshop 11.

The Shiny app is in `inst/app`. Run it from the repository root with:

```r
reactlog::reactlog_enable()
shiny::runApp("inst/app")
```

The workshop refactors the repeated reactive calculations, inspects the result
with `reactlog`, and adds a package function that launches the app.
