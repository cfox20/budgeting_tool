# Household Expense Tracker

This repository contains a local-first [Shiny](https://shiny.posit.co/) application for tracking household expenses. The app allows you to enter purchases manually, keep an editable table of all prior transactions, and generate category-based reports over custom date ranges.

## Features

- **Interactive data entry** – add new expenses with date, description, category, amount, and payer details.
- **Editable history table** – review and adjust previously entered expenses directly in the browser before saving.
- **Safe persistence** – expenses are stored in `data/expenses.csv`. When you save updates, the previous version is copied to `data/expenses_backup.csv` after you confirm the change.
- **Spending analytics** – filter by date range (defaults to the last month), exclude non-positive amounts, inspect totals by category, and view a bar chart of spending along with the detailed transaction list.
- **Budget planning** – capture multiple income sources, set monthly targets for each category, and track progress plus over/under budget categories in the reports tab.

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

## Data files

- `data/expenses.csv` – primary storage for all expense records. This file is ignored by Git so your personal data stays local.
- `data/expenses_backup.csv` – the most recent backup created right before you confirm a save. Review the modal preview carefully before overwriting the backup.
- `data/income_sources.csv` – saved list of your income sources and the amounts entered on the Budget Planning tab.
- `data/category_budget.csv` – saved monthly targets by category used to compare actual spending against your goals.

If either file does not exist yet, they will be created automatically after you add and save your first expense.

## Notes

- You can type new categories or payer names directly into the selectors; they will be remembered for future entries within the current session.
- Use the reporting tab to focus on specific time periods and to monitor the distribution of your spending.
- To restore from the backup manually, replace `data/expenses.csv` with `data/expenses_backup.csv` while the app is not running.

Enjoy tracking your expenses!
