# Household Expense Tracker

This repository contains a local-first [Shiny](https://shiny.posit.co/) application for tracking household expenses. The app allows you to enter purchases manually, keep an editable table of all prior transactions, and generate category-based reports over custom date ranges.

## Features

- **Interactive data entry** – add new expenses with date, description, category, amount, and payer details.
- **Editable history table** – review and adjust previously entered expenses directly in the browser before saving.
- **Safe persistence** – expenses are stored in `data/expenses.csv`. When you save updates, the previous version is copied to `data/expenses_backup.csv` after you confirm the change.
- **Spending analytics** – filter by date range (defaults to the last month), exclude non-positive amounts, inspect totals by category, and view a bar chart of spending along with the detailed transaction list.
- **Budget planning** – capture multiple income sources, set monthly targets for each category, and track progress plus over/under budget categories in the reports tab.
- **Desktop integration** – helper script can drop a ready-to-use desktop shortcut on Windows, macOS, or Linux (and will use your custom icon if you add one).


## Getting started

1. **Install R** (version 4.0 or newer is recommended).
2. **Install the required packages** once in your R session:

   ```r
   install.packages(c("shiny", "DT", "readr", "dplyr", "lubridate", "ggplot2", "scales"))
   ```

3. **Launch the app** from the project root:

   ```bash
   Rscript run_app.R
   ```

   This script checks for the required packages and then opens the Shiny app in your default web browser. Because the app is executed locally, your expense data never leaves your computer.

   If you set the environment variable `SHINY_LAUNCH_BROWSER=false` the script will skip opening a browser window, which is useful when embedding the app elsewhere.

4. **Run the desktop experience with Python (optional)**

   Install Python 3.9 or newer plus the lightweight [pywebview](https://pywebview.flowrl.com/) dependency:

   ```bash
   pip install pywebview
   ```

   Then start the bundled launcher which opens the Shiny app in a native desktop window while managing the underlying R process:

   ```bash
   python desktop_app.py
   ```

   The script automatically picks an available local port, waits for the Shiny server to become ready, and shuts it down again when you close the window. Use `python desktop_app.py --help` to see advanced options such as overriding the `Rscript` path.


## Data files

- `data/expenses.csv` – primary storage for all expense records. This file is ignored by Git so your personal data stays local.
- `data/expenses_backup.csv` – the most recent backup created right before you confirm a save. Review the modal preview carefully before overwriting the backup.
- `data/income_sources.csv` – saved list of your income sources and the amounts entered on the Budget Planning tab.
- `data/category_budget.csv` – saved monthly targets by category used to compare actual spending against your goals.
- `resources/icon_bytes.R` – raw byte vector that is converted into the Windows `.ico` file whenever the shortcut helper runs.
- `resources/budgeting_tool.ico` – generated Windows icon file consumed by the desktop shortcut generator (ignored by Git).



## Notes

## Acknowledgement

This project was entirely done using OpenAI's **Codex** and Google DeepMind's **Antigravity**.
