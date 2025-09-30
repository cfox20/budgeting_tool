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

5. *(Optional)* **Install a desktop shortcut** by running:

   ```bash
   Rscript tools/install_shortcut.R
   ```

   The script detects your operating system and places a launcher on your Desktop:

   - **Windows** – creates `Budgeting Tool.lnk` that runs `Rscript.exe --vanilla run_app.R` with the generated `resources/budgeting_tool.ico` icon.
   - **macOS** – writes a clickable `Budgeting Tool.command` shell script. If `app/www/icon.png` exists the script will copy it next to the shortcut so you can apply it manually (Get Info → drag the icon onto the preview).
   - **Linux** – creates an executable `budgeting-tool.desktop` entry. If `app/www/icon.png` is available it will be referenced; otherwise a generic system icon is used.

   Re-run the helper whenever you move the project folder so the shortcut points to the new location.


## Data files

- `data/expenses.csv` – primary storage for all expense records. This file is ignored by Git so your personal data stays local.
- `data/expenses_backup.csv` – the most recent backup created right before you confirm a save. Review the modal preview carefully before overwriting the backup.
- `data/income_sources.csv` – saved list of your income sources and the amounts entered on the Budget Planning tab.
- `data/category_budget.csv` – saved monthly targets by category used to compare actual spending against your goals.
- `resources/icon_bytes.R` – raw byte vector that is converted into the Windows `.ico` file whenever the shortcut helper runs.
- `resources/budgeting_tool.ico` – generated Windows icon file consumed by the desktop shortcut generator (ignored by Git).

### Optional app icon

The Shiny UI looks for `app/www/icon.png`. The file is omitted from the repository so you can manage your own artwork locally. To recreate the default icon that previously shipped with the project, save the Base64 payload below to a file and decode it:

```bash
cat <<'EOF' > /tmp/budgeting_tool_icon.b64
```

```
iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAABnElEQVR4nO3av0oDURTH8RfFEhsb
xQfxG8FgbSytrAXBiNgYWNjYKFsbaCsby1kpS/MNtmAx2ti6BQ1ZwWBr0oIhxiRx4u6773TuvTt0
sXJwX3nns5993x07rHdZ0z/9vWammZmZmZmZmZmZmZmZmfkx9fI/A79Z4bAb2LiP4GHDheTQRMsc
p0O47FNc7VTgmo9N44gncFlgM4/QlsNsT9cQx7FdQI6zXUC2cQ2gbrMNsH6zDXA+s91wfrMdcF6z
DXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81wfrMd
cF6zDXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81w
frMdcF6zDXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81wfrMdcF6zDXB+s81wf7P9w4PzcPE7
soT8Dx79hwHQeG4xB5nqPdqsVrm8CL08LFQ/8XMpgfrF2BDxCqYH6xtQp4B6xtQR4D6xtQd4A6xt
Q94BqxtQ94D6xtQd4A6xtQ94BqxtQ94D6xtQd4A6xtQ94BqxtQ94D6xtQd4A6xtQ94BqxtQ94D6x
tQd4A6xtQ94BqxtQ94D6xtQd4A6xtQ94BqxtQ94D6xtQd4A6xtQ94BqxtQ94P7N/WZfHXj8w9QAA
AABJRU5ErkJggg==
```

```bash
EOF
base64 --decode /tmp/budgeting_tool_icon.b64 > app/www/icon.png
```

Feel free to replace the resulting PNG with your own design; it will automatically be used by the UI and the shortcut helper when present.


If either file does not exist yet, they will be created automatically after you add and save your first expense.

### Sample data

For convenience the repository ships with an example data set that represents a
couple living on roughly $90k per year (about $7,750 in combined net monthly
income). You can explore the app immediately with the included March 2024
transactions, income sources, and budget targets. When you're ready to start
using your own numbers, delete the CSV files in the `data/` directory (or move
them elsewhere) and the app will recreate fresh, empty versions the next time it
launches.

## Notes

- You can type new categories or payer names directly into the selectors; they will be remembered for future entries within the current session.
- Use the reporting tab to focus on specific time periods and to monitor the distribution of your spending.
- To restore from the backup manually, replace `data/expenses.csv` with `data/expenses_backup.csv` while the app is not running.

Enjoy tracking your expenses!
