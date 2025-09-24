args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[[1]]) else NA_integer_
if (is.na(port) || port <= 0) {
  port <- 0L
}

required_packages <- c(
  "shiny",
  "shinythemes",
  "DT",
  "readr",
  "dplyr",
  "tidyr",
  "lubridate",
  "ggplot2",
  "scales"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing) > 0) {
  repos <- getOption("repos")
  if (is.null(repos) || identical(repos, structure("@CRAN@", .Names = "CRAN"))) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  install.packages(missing, repos = repos, quiet = TRUE)
}

options(shiny.port = port, shiny.host = "127.0.0.1")
options(shiny.launch.browser = FALSE)

shiny::runApp(appDir = ".", host = "127.0.0.1", port = port, launch.browser = FALSE)
