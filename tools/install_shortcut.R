#!/usr/bin/env Rscript

# ------------------------------- helpers ------------------------------------
current_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  matches <- grep("^--file=", args)
  if (length(matches) == 0) {
    stop("Unable to determine script location; please run via Rscript.")
  }
  normalizePath(sub("^--file=", "", args[matches[length(matches)]]), winslash = "/", mustWork = TRUE)
}

find_python <- function(candidates) {
  for (candidate in candidates) {
    resolved <- Sys.which(candidate)
    if (!is.na(resolved) && nzchar(resolved)) {
      return(normalizePath(resolved, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Python 3 was not found on PATH.")
}

ensure_icon <- function(path) {
  if (!file.exists(path)) {
    stop("Logo not found at ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# ------------------------------- setup ---------------------------------------
script_path <- current_script()
repo_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
runner <- file.path(repo_dir, "desktop_app.py")
if (!file.exists(runner)) {
  stop("Could not find desktop_app.py at ", runner)
}
icon_path <- ensure_icon(file.path(repo_dir, "resources", "logo.ico"))

desktop_dir <- if (startsWith(tolower(Sys.info()["sysname"]), "win")) {
  file.path(Sys.getenv("USERPROFILE"), "Desktop")
} else {
  path.expand("~/Desktop")
}
if (!dir.exists(desktop_dir)) {
  stop("Desktop folder not found at ", desktop_dir)
}

python_candidates <- if (startsWith(tolower(Sys.info()["sysname"]), "win")) {
  c("pythonw.exe", "python.exe")
} else {
  c("python3", "python")
}
python_bin <- find_python(python_candidates)

sysname <- tolower(Sys.info()["sysname"])

# ------------------------------- windows -------------------------------------
if (startsWith(sysname, "win")) {
  shortcut_path <- normalizePath(file.path(desktop_dir, "Budgeting Tool.lnk"), winslash = "\\", mustWork = FALSE)
  target <- normalizePath(python_bin, winslash = "\\", mustWork = TRUE)
  working_dir <- normalizePath(repo_dir, winslash = "\\", mustWork = TRUE)
  runner_win <- normalizePath(runner, winslash = "\\", mustWork = TRUE)
  icon_win <- normalizePath(icon_path, winslash = "\\", mustWork = TRUE)
  ps_lines <- c(
    "$ErrorActionPreference = 'Stop'",
    "$shell = New-Object -ComObject WScript.Shell",
    sprintf('$shortcut = $shell.CreateShortcut("%s")', shortcut_path),
    sprintf('$shortcut.TargetPath = "%s"', target),
    sprintf('$shortcut.Arguments = "%s"', runner_win),
    sprintf('$shortcut.WorkingDirectory = "%s"', working_dir),
    sprintf('$shortcut.IconLocation = "%s"', icon_win),
    '$shortcut.WindowStyle = 7',
    '$shortcut.Save()'
  )
  tmp <- tempfile(fileext = ".ps1")
  writeLines(ps_lines, tmp, useBytes = TRUE)
  on.exit(unlink(tmp), add = TRUE)
  res <- system2("powershell", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(tmp)), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
    stop("Failed to create Windows shortcut: ", paste(res, collapse = "\n"))
  }
  message("Shortcut created at ", shortcut_path)
  quit(status = 0)
}

# ------------------------------- macos ---------------------------------------
if (sysname == "darwin") {
  shortcut_path <- file.path(desktop_dir, "Budgeting Tool.command")
  script_lines <- c(
    "#!/bin/bash",
    sprintf('cd "%s"', repo_dir),
    sprintf('"%s" "%s"', python_bin, basename(runner))
  )
  writeLines(script_lines, shortcut_path, useBytes = TRUE)
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
launch_command <- sprintf("cd %s && %s %s", sh_quote(repo_dir), sh_quote(python_bin), sh_quote(runner))
exec_line <- sprintf('bash -lc "%s"', gsub('([\\"$`])', '\\\\1', launch_command, perl = TRUE))
desktop_entry <- c(
  "[Desktop Entry]",
  "Type=Application",
  "Name=Budgeting Tool",
  "Comment=Launch the budgeting tool",
  sprintf("Exec=%s", exec_line),
  sprintf("Path=%s", repo_dir),
  sprintf("Icon=%s", icon_path),
  "Terminal=false",
  "Categories=Office;Finance;"
)
writeLines(desktop_entry, shortcut_path, useBytes = TRUE)
Sys.chmod(shortcut_path, mode = "0755")
message("Desktop launcher created at ", shortcut_path)
