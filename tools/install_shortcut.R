#!/usr/bin/env Rscript

current_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  matches <- grep("^--file=", args)
  if (length(matches) == 0) {
    stop("Unable to determine script location; please run via Rscript.")
  }
  normalizePath(sub("^--file=", "", args[matches[length(matches)]]), winslash = "/", mustWork = TRUE)
}

ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Package '", pkg, "' not found; attempting to install from CRAN...")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }

  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' could not be loaded; please install it manually and re-run this script.")
  }
}

stop_if_not_windows <- function() {
  if (!startsWith(tolower(Sys.info()["sysname"]), "win")) {
    stop("RInno can only build Windows installers. Please run this script on Windows.")
  }
}

script_path <- current_script()
repo_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
app_dir <- file.path(repo_dir, "app")
app_entry <- file.path(repo_dir, "run_app.R")
icon_path <- file.path(repo_dir, "resources", "logo.ico")
installer_dir <- file.path(repo_dir, "installer")
app_name <- "BudgetingTool"

if (!dir.exists(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
  stop("The Shiny application directory was not found at ", app_dir)
}

if (!file.exists(app_entry)) {
  stop("Launcher script not found at ", app_entry)
}

if (!file.exists(icon_path)) {
  stop("Logo not found at ", icon_path)
}

dir.create(installer_dir, showWarnings = FALSE, recursive = TRUE)

stop_if_not_windows()
ensure_package("RInno")

pkgs <- c("shiny", "DT", "readr", "dplyr", "tidyr", "lubridate", "ggplot2", "scales")

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing application packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

message("Ensuring Inno Setup is available...")
RInno::install_inno()

message("Configuring RInno project...")
RInno::setup_app(
  app_name = app_name,
  app_dir = app_dir,
  dir_out = installer_dir,
  pkgs = pkgs,
  include_R = TRUE,
  app_icon = icon_path,
  app_desc = "Household budgeting tool",
  files = c(app_entry, file.path(repo_dir, "desktop_app.py"), file.path(repo_dir, "resources"))
)

message("Building installer with Inno Setup...")
RInno::compile_iss()

message("RInno finished building the installer. Check the '", installer_dir, "' folder for the .exe file.")
