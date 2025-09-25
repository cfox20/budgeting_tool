# Household Budgeting Desktop App

This repository now ships a standalone desktop application for tracking a
household budget while letting you build the interface with familiar
[R Shiny](https://shiny.posit.co/) components. A small Python launcher embeds the
Shiny app inside a native desktop window so you still get a double-clickable
experience, but you can customise the UI and server logic directly in `r_app/`.

## Highlights

- **Desktop-first experience** powered by a lightweight PySide6 wrapper around a
  Shiny application. Launch it like any other native program while developing in
  R.
- **Quick data entry** with Shiny inputs for dates, descriptions, categories,
  sub-categories, payers, accounts, and amounts plus an editable table of every
  previous transaction.

- **Safe persistence** that keeps an on-disk CSV plus an automatically managed
  backup. You inspect a preview before every save so accidental overwrites are
  less likely.
- **Budget planning tab** where you configure income sources and monthly targets
  per category and sub-category. These feed directly into the reporting tools.
- **Interactive reports** to explore spending by category over any time window,
  visualise how much of each budget has been consumed, trend spending over time,
  and list categories that are over or under budget.
- **Offline storage** – when running from source the app keeps everything in
  `user_data/` (ignored by Git) so your finances stay private while remaining
  easy to back up or edit with another tool.


## Prerequisites

1. **Install Python 3.10 or newer.** The wrapper uses modern typing features and
   PySide6 builds for current Python releases.
2. **Install R 4.2 or newer** and ensure the `Rscript` command is available on
   your PATH. The first launch installs any required R packages automatically.
3. **Create a virtual environment (recommended):**

   ```bash
   python -m venv .venv
   source .venv/bin/activate  # Windows: .venv\Scripts\activate
   ```

4. **Install the Python dependencies:**

   ```bash
   pip install -r requirements.txt
   ```

## Running the app

From the project root, launch the desktop window with:

```bash
python run_desktop.py
```

The first run creates the data directory, seeds it with example expenses, income
sources, and category budgets, and installs the required R packages (Shiny,
tidyverse components, DT, etc.) inside your local R
environment. You can delete or edit those rows at any point; new empty files will
be generated automatically if the CSVs are removed.

When running from source the directory lives at `user_data/` inside the
repository. Frozen builds created by PyInstaller store the files in the
platform-specific locations listed in [Data directory](#data-directory) so the
app always writes to a user-writable folder.

If the Shiny process ever fails to boot you will see an error dialog. Consult
`<data directory>/shiny_app.log` (see [Data directory](#data-directory) for the
exact path) for the full R console output.

## Building a desktop installer / executable

You can bundle the application into a platform-specific executable using
[PyInstaller](https://pyinstaller.org/):

1. Make sure PyInstaller is available:

   ```bash
   pip install pyinstaller
   ```

2. Run the helper script:

   ```bash
   python installer/build_installer.py
   ```

   PyInstaller places the results in the `installer/dist/` directory. On Windows you will
   find a `BudgetingTool` folder containing `BudgetingTool.exe`. On macOS and
   Linux you receive a similar bundled executable or launcher script depending on
   the platform. The bundle includes the Shiny sources from `r_app/`; you still
   need R installed on the target machine so the wrapped process can run.

   The script also zips the bundle into a timestamped archive such as
   `BudgetingTool-windows-20240101.zip`. Upload that archive to your repository's
   releases page and share a direct download link like:

   ```
   https://github.com/<your-account>/budgeting_tool/releases/latest/download/BudgetingTool-windows-20240101.zip
   ```

   Replace `<your-account>` and the archive name with the values that match your
   release. Anyone with that URL can download the ready-to-run bundle without
   cloning the repository.

3. (Optional, Windows) Generate a traditional installer with a Start Menu entry
   and an optional desktop shortcut using
   [Inno Setup](https://jrsoftware.org/isinfo.php):

   1. Install Inno Setup.
   2. Run PyInstaller as shown above so `installer/dist/BudgetingTool` exists.
   3. Open `installer/windows_installer.iss` in Inno Setup and build it. The
      script outputs `installer/dist/BudgetingToolSetup.exe`.

   During installation the user can tick **Create a desktop icon**. The installer
   also adds a Start Menu shortcut and offers to launch the budgeting tool when
   the wizard finishes.

## Application overview

- **Expenses tab** – enter new purchases while keeping an editable grid of all
  existing records. Double-click any cell (including the category and sub-category)
  to adjust it. When you save, the current log is written to `expenses.csv` in the
  data directory and the prior version is copied to `expenses_backup.csv` after
  you confirm the preview dialog.
- **Budget planning tab** – manage your recurring income sources and per-category
  and sub-category spending targets. Click *Save* in each section to persist those
  choices to the corresponding CSV files in the data directory.
- **Reports tab** – pick a date range (defaulting to the last 30 days) to see
  how much you have spent in each category, how it compares with your target, a
  percent-of-budget progress view, per-category spending trends over time, and
  explicit lists of categories that are currently over or under budget.

## Data directory

The application keeps its CSVs in a user-specific location:

| How you're running | Location |
| ------------------- | -------- |
| From source (``python run_desktop.py``) | `user_data/` inside the repository |
| Frozen bundle / installer on Windows | `%LOCALAPPDATA%\Budgeting Tool` |
| Frozen bundle on macOS | `~/Library/Application Support/Budgeting Tool` |
| Frozen bundle on Linux | `$XDG_DATA_HOME/Budgeting Tool` (falls back to `~/.local/share/Budgeting Tool`) |

All files in that directory are part of your personal dataset and are not
tracked by Git:

- `expenses.csv` – the primary ledger of every purchase you enter.
- `expenses_backup.csv` – last version of the ledger captured just before your
  most recent save.
- `income_sources.csv` – your configured household income streams with monthly
  amounts.
- `category_budget.csv` – monthly target amounts per spending category and sub-category.

Feel free to open these CSVs with Excel or another spreadsheet tool if you need
bulk edits. The application reads the latest values each time you switch tabs or
refresh the reports.

## Customising the app

- Edit `r_app/app.R` to adjust the UI, add new analysis panels, or integrate
  other R packages. The Python wrapper simply launches whatever Shiny app lives
  in that folder.
- Add, rename, or remove categories directly in the Expenses or Budget tabs; the
  drop-downs update as soon as you save changes.
- Replace the seeded data by deleting the CSV files in the data directory while
  the application is closed. Launching the app again will create fresh empty files.
- If you want to distribute the tool to another computer, copy the repository or
  use the PyInstaller bundle and then copy your data directory alongside it.

## Troubleshooting

- If the desktop window fails to launch, confirm that both Python and R are
  installed and that `Rscript` is on your PATH.
- When building with PyInstaller, ensure you run the command from an activated
  virtual environment that already has PySide6 and pandas installed, and that the
  target machine also has R available.
- To reset the app, close it and delete the data directory. The next launch

  recreates it with the default sample records.

Enjoy budgeting with a fully local desktop experience powered by R Shiny!
