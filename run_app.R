#!/usr/bin/env Rscript

required_packages <- c(
  "shiny",
  "DT",
  "readr",
  "dplyr",
  "tidyr",
  "lubridate",
  "ggplot2",
  "scales",
  "stringdist",
  "shinyjs"
)
missing <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )
]

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(plotly::layout)

if (length(missing) > 0) {
  message(
    "The following packages need to be installed before running the app: ",
    paste(missing, collapse = ", ")
  )
  message(
    "You can install them with:\ninstall.packages(c(\"",
    paste(missing, collapse = "\", \""),
    "\"))"
  )
  quit(status = 1)
}

host <- Sys.getenv("SHINY_HOST", unset = "127.0.0.1")
port_env <- Sys.getenv("SHINY_PORT", unset = "")
launch_browser_env <- tolower(Sys.getenv(
  "SHINY_LAUNCH_BROWSER",
  unset = "true"
))

port <- if (nzchar(port_env)) {
  suppressWarnings(as.integer(port_env))
} else {
  NULL
}

if (!is.null(port) && (is.na(port) || port <= 0)) {
  stop("Invalid SHINY_PORT value: ", port_env)
}

launch_browser <- if (launch_browser_env %in% c("false", "0", "no")) {
  FALSE
} else {
  TRUE
}

message("Launching the Household Expense Tracker...")
shiny::runApp(
  appDir = "app",
  launch.browser = launch_browser,
  host = host,
  port = port
)
