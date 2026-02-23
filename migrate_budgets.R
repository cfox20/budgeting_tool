library(readr)
library(dplyr)

budgets_path <- "data/category_budget.csv"
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- paste0("data/category_budget_backup_", timestamp, ".csv")

if (file.exists(budgets_path)) {
    # Create backup
    if (file.copy(budgets_path, backup_path)) {
        message(paste("Success: Backup created at", backup_path))
    } else {
        stop("Error: Failed to create backup. Migration aborted.")
    }

    df <- read_csv(budgets_path, show_col_types = FALSE)

    if (!"EffectiveDate" %in% names(df)) {
        df <- df %>%
            mutate(EffectiveDate = as.Date("2020-01-01")) %>%
            select(EffectiveDate, everything())

        write_csv(df, budgets_path)
        message("Migration successful: Added EffectiveDate column.")
    } else {
        message("Migration skipped: EffectiveDate already exists.")
    }
} else {
    message("File not found.")
}
