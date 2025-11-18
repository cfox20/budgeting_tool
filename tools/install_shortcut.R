#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
script_path <- NULL
for (arg in args) {
  if (startsWith(arg, "--file=")) {
    script_path <- sub("^--file=", "", arg)
    break
  }
}
if (is.null(script_path)) {
  stop("Unable to determine script location; please run via Rscript.")
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
runner <- file.path(repo_dir, "desktop_app.py")
if (!file.exists(runner)) {
  stop("Could not find desktop_app.py at ", runner)
}

icon_ico_path <- file.path(repo_dir, "resources", "logo.ico")
if (!file.exists(icon_ico_path)) {
  stop("Could not find logo at ", icon_ico_path)
}
# icon_bytes_source <- file.path(repo_dir, "resources", "icon_bytes.R")

# if (!file.exists(icon_bytes_source)) {
#   stop("Icon byte source not found at ", icon_bytes_source, ".")
# }

# source(icon_bytes_source)

# if (!exists("budgeting_tool_icon_bytes")) {
#   stop("budgeting_tool_icon_bytes() was not defined after sourcing ", icon_bytes_source, ".")
# }

# ico_bytes <- budgeting_tool_icon_bytes()
# if (!inherits(ico_bytes, "raw")) {
#   stop("budgeting_tool_icon_bytes() must return a raw vector.")
# }

# existing_size <- suppressWarnings(file.size(icon_ico_path))
# if (is.na(existing_size) || existing_size != length(ico_bytes)) {
#   dir.create(dirname(icon_ico_path), showWarnings = FALSE, recursive = TRUE)
#   writeBin(ico_bytes, icon_ico_path, useBytes = TRUE)
# }

icon_ico <- normalizePath(icon_ico_path, winslash = "/", mustWork = TRUE)

desktop_path <- NULL
sysname <- tolower(Sys.info()["sysname"])
if (startsWith(sysname, "win")) {
  desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
} else {
  desktop_path <- path.expand("~/Desktop")
}
if (!dir.exists(desktop_path)) {
  stop("Desktop folder not found at ", desktop_path, ". Please create it or adjust the script.")
}

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
    sprintf('$Shortcut.Arguments = "\"%s\""', runner_win),
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
  repo_escaped <- gsub('"', '\\"', repo_dir, fixed = TRUE)
  exec_cmd <- sprintf(
    'bash -c "cd \"%s\" && if command -v python3 >/dev/null 2>&1; then PY=python3; elif command -v python >/dev/null 2>&1; then PY=python; else echo Python 3 is required but was not found.; exit 1; fi; exec \"$PY\" desktop_app.py"',
    repo_escaped
  )
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
