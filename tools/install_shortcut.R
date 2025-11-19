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

if (startsWith(sysname, "win")) {
  shortcut_path <- normalizePath(file.path(desktop_path, "Budgeting Tool.lnk"), winslash = "\\", mustWork = FALSE)
  find_python <- function() {
    candidates <- c("pythonw.exe", "python.exe")
    for (candidate in candidates) {
      resolved <- Sys.which(candidate)
      if (!is.na(resolved) && nzchar(resolved)) {
        return(normalizePath(resolved, winslash = "\\", mustWork = TRUE))
      }
    }
    stop("Could not find python.exe or pythonw.exe on PATH. Please install Python 3.")
  }
  target <- find_python()
  working_dir <- normalizePath(repo_dir, winslash = "\\", mustWork = TRUE)
  runner_win <- normalizePath(runner, winslash = "\\", mustWork = TRUE)
  icon_win <- normalizePath(icon_ico, winslash = "\\", mustWork = TRUE)
  ps_lines <- c(
    "$ErrorActionPreference = 'Stop'",
    "$WshShell = New-Object -ComObject WScript.Shell",
    sprintf('$Shortcut = $WshShell.CreateShortcut("%s")', shortcut_path),
    sprintf('$Shortcut.TargetPath = "%s"', target),
    sprintf('$Shortcut.WorkingDirectory = "%s"', working_dir),
    sprintf('$Shortcut.IconLocation = "%s"', icon_win),
    # <-- change this line:
    sprintf('$Shortcut.Arguments = "%s"', runner_win),
    "$Shortcut.Save()"
  )
  tmp <- tempfile(fileext = ".ps1")
  writeLines(ps_lines, tmp, useBytes = TRUE)
  on.exit(unlink(tmp), add = TRUE)
  res <- system2("powershell", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(tmp)), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
    stop("Failed to create Windows shortcut: ", paste(res, collapse = "\n"))
  }
  message("Shortcut created at ", shortcut_path)
} else if (sysname == "darwin") {
  shortcut_path <- file.path(desktop_path, "Budgeting Tool.command")
  lines <- c(
    "#!/bin/bash",
    sprintf('cd "%s"', repo_dir),
    'PYTHON_BIN="$(command -v python3)"',
    'if [ -z "$PYTHON_BIN" ]; then',
    '  PYTHON_BIN="$(command -v python)"',
    'fi',
    'if [ -z "$PYTHON_BIN" ]; then',
    '  echo "Python 3 is required but was not found on PATH."',
    '  exit 1',
    'fi',
    '"$PYTHON_BIN" desktop_app.py'
  )
  writeLines(lines, shortcut_path, useBytes = TRUE)
  Sys.chmod(shortcut_path, mode = "0755")
  icon_dest <- file.path(desktop_path, "Budgeting Tool.ico")
  file.copy(icon_ico_path, icon_dest, overwrite = TRUE)
  message("Shortcut script created at ", shortcut_path)
  message("macOS does not support applying .ico icons automatically; a copy of the logo was saved next to the shortcut so you can assign it manually via Get Info → drag the icon.")
} else {
  shortcut_path <- file.path(desktop_path, "budgeting-tool.desktop")
  python_bin <- NULL
  for (candidate in c("python3", "python")) {
    resolved <- Sys.which(candidate)
    if (!is.na(resolved) && nzchar(resolved)) {
      python_bin <- normalizePath(resolved, winslash = "/", mustWork = TRUE)
      break
    }
  }
  if (is.null(python_bin)) {
    stop("Python 3 is required but was not found on PATH.")
  }
  quote_arg <- function(x) {
    x <- normalizePath(x, winslash = "/", mustWork = TRUE)
    sprintf('"%s"', gsub('"', '\\"', x, fixed = TRUE))
  }
  exec_cmd <- sprintf(
    "%s %s",
    quote_arg(python_bin),
    quote_arg(runner)
  )
  exec_cmd <- sprintf("bash -lc %s", sh_quote(launch_cmd))
  icon_entry <- sprintf("Icon=%s", icon_ico)
  desktop_entry <- c(
    "[Desktop Entry]",
    "Type=Application",
    "Name=Budgeting Tool",
    "Comment=Launch the Household Expense Tracker",
    sprintf("Exec=%s", exec_cmd),
    sprintf("Path=%s", repo_dir),
    icon_entry,
    "Terminal=false",
    "Categories=Office;Finance;"
  )
  writeLines(desktop_entry, shortcut_path, useBytes = TRUE)
  Sys.chmod(shortcut_path, mode = "0755")
  message("Desktop launcher created at ", shortcut_path)
}
