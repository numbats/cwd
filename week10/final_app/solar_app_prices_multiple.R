# Legacy entry point retained for existing links.
# The maintained complete dashboard now lives at week10/app.R.

app_candidates <- c(
  file.path("..", "app.R"),
  file.path("week10", "app.R")
)
app_file <- app_candidates[file.exists(app_candidates)][1]

if (is.na(app_file)) {
  stop("Could not locate week10/app.R.")
}

source(app_file, chdir = TRUE)$value
