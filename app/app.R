library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(lubridate)
library(stringdist)
library(shinyjs)


# Data configuration -----------------------------------------------------------

data_dir <- normalizePath(
  file.path("..", "data"),
  winslash = "/",
  mustWork = FALSE
)
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}

expenses_path <- file.path(data_dir, "expenses.csv")
budgets_path <- file.path(data_dir, "category_budget.csv")
income_path <- file.path(data_dir, "income_sources.csv")
goals_path <- file.path(data_dir, "goals.csv")
goal_progress_path <- file.path(data_dir, "goal_progress.csv")


empty_expenses <- tibble::tibble(
  Date = as.Date(character()),
  Description = character(),
  Category = character(),
  Subcategory = character(),
  Amount = numeric(),
  Payer = character()
)

empty_budgets <- tibble::tibble(
  Category = character(),
  Subcategory = character(),
  Limit = numeric(),
  Frequency = character(),
  EffectiveDate = as.Date(character()),
  ConclusionDate = as.Date(character())
)

empty_goals <- tibble::tibble(
  Goal = character(),
  TargetAmount = numeric(),
  TargetMonth = as.Date(character()),
  CreatedDate = as.Date(character())
)

empty_goal_progress <- tibble::tibble(
  Goal = character(),
  Month = as.Date(character()),
  Saved = logical()
)


default_payers <- c("Joint", "Carson", "Chloe")

clean_subcategory <- function(x) {
  if (is.null(x)) {
    return("")
  }

  x <- tidyr::replace_na(x, "")
  trimws(x)
}

display_subcategory <- function(value) {
  value <- clean_subcategory(value)
  ifelse(nzchar(value), value, "")
}

load_expenses <- function() {
  if (!file.exists(expenses_path)) {
    return(empty_expenses)
  }

  df <- readr::read_csv(
    expenses_path,
    col_types = readr::cols(
      Date = col_date(),
      Description = col_character(),
      Category = col_character(),
      Subcategory = col_character(),
      Amount = col_double(),
      Payer = col_character()
    ),
    show_col_types = FALSE
  )

  if (!"Subcategory" %in% names(df)) {
    df$Subcategory <- ""
  }

  df %>%
    mutate(
      Description = tidyr::replace_na(Description, ""),
      Category = tidyr::replace_na(Category, ""),
      Subcategory = clean_subcategory(Subcategory),
      Payer = tidyr::replace_na(Payer, ""),
      Amount = replace_na(Amount, 0)
    ) %>%
    arrange(desc(Date))
}

load_budgets <- function() {
  if (!file.exists(budgets_path)) {
    return(empty_budgets)
  }

  df <- readr::read_csv(
    budgets_path,
    col_types = cols(
      Category = col_character(),
      Subcategory = col_character(),
      Limit = col_double(),
      Frequency = col_character(),
      EffectiveDate = col_date(),
      ConclusionDate = col_date()
    ),
    show_col_types = FALSE
  )

  if (!"Limit" %in% names(df) && "Target" %in% names(df)) {
    df <- dplyr::rename(df, Limit = Target)
  }

  if (!"EffectiveDate" %in% names(df)) {
    df$EffectiveDate <- as.Date("2020-01-01")
  }

  if (!"ConclusionDate" %in% names(df)) {
    df$ConclusionDate <- as.Date(NA)
  }

  df <- df %>%
    mutate(
      Category = tidyr::replace_na(Category, ""),
      Subcategory = clean_subcategory(Subcategory),
      Limit = replace_na(Limit, 0),
      Frequency = replace_na(Frequency, "Monthly"),
      EffectiveDate = replace_na(EffectiveDate, as.Date("2020-01-01")),
      ConclusionDate = as.Date(ConclusionDate)
    )

  calculate_budget_conclusions(df)
}

calculate_budget_conclusions <- function(df) {
  df %>%
    arrange(Category, Subcategory, EffectiveDate) %>%
    group_by(Category, Subcategory) %>%
    mutate(
      ConclusionDate = lead(EffectiveDate) - days(1)
    ) %>%
    ungroup() %>%
    arrange(Category, Subcategory, desc(EffectiveDate))
}

load_monthly_income <- function() {
  if (!file.exists(income_path)) {
    return(NA_real_)
  }

  income <- readr::read_csv(
    income_path,
    col_types = cols(
      Source = col_character(),
      Amount = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    mutate(Amount = replace_na(Amount, 0)) %>%
    summarise(total = sum(Amount, na.rm = TRUE)) %>%
    pull(total)

  if (is.finite(income) && length(income) == 1) {
    income
  } else {
    NA_real_
  }
}

write_expenses <- function(df) {
  readr::write_csv(df, expenses_path, na = "")
}

write_budgets <- function(df) {
  readr::write_csv(df, budgets_path, na = "")
}

write_monthly_income <- function(amount) {
  tibble::tibble(Source = "Monthly", Amount = amount) %>%
    readr::write_csv(income_path, na = "")
}

load_goals <- function() {
  if (!file.exists(goals_path)) {
    return(empty_goals)
  }
  readr::read_csv(
    goals_path,
    col_types = cols(
      Goal = col_character(),
      TargetAmount = col_double(),
      TargetMonth = col_date(),
      CreatedDate = col_date()
    ),
    show_col_types = FALSE
  )
}

write_goals <- function(df) {
  readr::write_csv(df, goals_path, na = "")
}

load_goal_progress <- function() {
  if (!file.exists(goal_progress_path)) {
    return(empty_goal_progress)
  }
  readr::read_csv(
    goal_progress_path,
    col_types = cols(
      Goal = col_character(),
      Month = col_date(),
      Saved = col_logical()
    ),
    show_col_types = FALSE
  )
}

write_goal_progress <- function(df) {
  readr::write_csv(df, goal_progress_path, na = "")
}


format_subcategory <- function(value) {
  value <- clean_subcategory(value)
  ifelse(nzchar(value), value, "(Unspecified)")
}

format_expense_table_data <- function(df) {
  df %>% mutate(Subcategory = display_subcategory(Subcategory))
}

format_budget_table_data <- function(df) {
  df %>% mutate(Subcategory = display_subcategory(Subcategory))
}

get_monthly_limit <- function(limit, frequency) {
  case_when(
    frequency == "Monthly" ~ limit,
    frequency == "Quarterly" ~ limit / 3,
    frequency == "Bi-annually" ~ limit / 6,
    frequency == "Annually" ~ limit / 12,
    TRUE ~ limit
  )
}



# User interface --------------------------------------------------------------

ui <- navbarPage(
  title = "Household Budgeting",
  tabPanel(
    "Expenses",
    fluidPage(
      useShinyjs(),
      fluidRow(
        column(
          width = 4,
          h3("Log an expense"),
          dateInput("expense_date", "Date", value = Sys.Date()),
          textInput("expense_description", "Description"),
          selectizeInput(
            "expense_category",
            "Category",
            choices = NULL,
            options = list(
              placeholder = "Select or add a category",
              create = TRUE
            )
          ),
          selectizeInput(
            "expense_subcategory",
            "Subcategory",
            choices = NULL,
            options = list(
              placeholder = "Select or add a subcategory",
              create = TRUE,
              allowEmptyOption = TRUE
            )
          ),
          numericInput(
            "expense_amount",
            "Amount",
            value = NA,
            min = 0,
            step = 0.01
          ),
          selectizeInput(
            "expense_payer",
            "Payer",
            choices = NULL,
            options = list(placeholder = "Select or add a payer", create = TRUE)
          ),
          actionButton("add_expense", "Add expense", class = "btn-primary"),
          br(),
          br(),
          strong("Totals"),
          textOutput("expense_totals", inline = FALSE)
        ),
        column(
          width = 8,
          h3("Recorded expenses"),
          actionButton(
            "delete_expense",
            "Delete selected expense",
            class = "btn-danger mb-2"
          ),
          DTOutput("expense_table")
        )
      )
    )
  ),
  tabPanel(
    "Budgeting",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Plan budgets"),
          numericInput(
            "income",
            "Monthly income",
            value = NA,
            min = 0,
            step = 50
          ),
          actionButton("set_income", "Save income", class = "btn-secondary"),
          br(),
          br(),
          h4("Add or update a budget line"),
          dateInput(
            "budget_start",
            "Effective Month",
            value = floor_date(Sys.Date(), "month"),
            format = "yyyy-mm-dd",
            startview = "year"
          ),
          selectizeInput(
            "budget_category",
            "Category",
            choices = NULL,
            options = list(
              placeholder = "Select or add a category",
              create = TRUE
            )
          ),
          selectizeInput(
            "budget_subcategory",
            "Subcategory",
            choices = NULL,
            options = list(
              placeholder = "Select or add a subcategory",
              create = TRUE,
              allowEmptyOption = TRUE
            )
          ),
          selectizeInput(
            "budget_frequency",
            "Frequency",
            choices = c("Monthly", "Quarterly", "Bi-annually", "Annually"),
            selected = "Monthly"
          ),
          numericInput(
            "budget_limit",
            "Limit (per frequency period)",
            value = NA,
            min = 0,
            step = 10
          ),
          actionButton("add_budget", "Save budget", class = "btn-primary")
        ),
        column(
          width = 8,
          h3("Current Budgets"),
          uiOutput("income_summary"),
          div(
            class = "btn-group",
            style = "margin-bottom: 10px;",
            actionButton("update_budget_btn", "Update Selected", class = "btn-info"),
            actionButton(
              "delete_budget",
              "Delete Selected",
              class = "btn-danger"
            )
          ),
          DTOutput("budget_table"),
          hr(),
          h3("Future Budgets"),
          DTOutput("future_budget_table"),
          hr(),
          h3("Suggested Budgets"),
          p("Showing suggestions for monthly budgets where the current month's spending deviated from your budget by more than $50. Based on two Weighted Moving Average options of recent spending: Hasty (0.6, 0.3, 0.1) and Conservative (0.4, 0.4, 0.2)."),
          div(
            class = "btn-group",
            style = "margin-bottom: 10px;",
            actionButton("apply_hasty_btn", "Apply Hasty Suggestions", class = "btn-warning"),
            actionButton("apply_conservative_btn", "Apply Conservative Suggestions", class = "btn-success")
          ),
          DTOutput("suggested_budget_table")
        )
      )
    )
  ),
  tabPanel(
    "Goals",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Manage Goals"),
          textInput("goal_name", "Goal Name"),
          numericInput("goal_target", "Target Amount ($)", value = NA, min = 0),
          dateInput(
            "goal_target_month",
            "Target Month",
            value = floor_date(Sys.Date() + months(1), "month"),
            format = "yyyy-mm-dd",
            startview = "year"
          ),
          actionButton("add_goal", "Save Goal", class = "btn-primary"),
          br(),
          br(),
          h4("Monthly Summary"),
          uiOutput("goals_monthly_summary")
        ),
        column(
          width = 8,
          h3("Your Goals"),
          uiOutput("goals_list_ui")
        )
      )
    )
  ),
  tabPanel(
    "Reporting",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          h3("Budget performance"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                "report_month",
                "Period",
                choices = c("All time" = "all"),
                selected = "all"
              )
            ),
            column(
              width = 5,
              textInput("report_email_to", "Email to:", placeholder = "recipient@example.com")
            ),
            column(
              width = 3,
              style = "margin-top: 25px;",
              actionButton("email_report", "Generate & Email Report", class = "btn-primary", icon = icon("envelope"))
            )
          ),
          uiOutput("report_summary"),
          DTOutput("report_table"),
          br(),
          h3("Spending trends"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                "spending_period",
                "Time aggregation",
                choices = c("Monthly" = "month", "Weekly" = "week"),
                selected = "month"
              )
            ),
            column(
              width = 4,
              selectInput(
                "spending_view",
                "View",
                choices = c(
                  "Total spending" = "total",
                  "By category" = "category"
                ),
                selected = "total"
              )
            ),
            column(
              width = 4,
              uiOutput("trend_category_filter")
            )
          ),
          plotlyOutput("spending_trend", height = "400px"),
          br(),
          h3("Spending by category"),
          plotOutput("category_plot", height = "350px"),
          br(),
          h3("Category and subcategory details"),
          DTOutput("category_table")
        )
      )
    )
  ),
  tabPanel(
    "Settings",
    fluidPage(
      column(
        width = 12,
        h3("Data Management"),
        p("Create a timestamped backup of your current expenses and budget data."),
        actionButton(
          "backup_data",
          "Backup Data",
          icon = icon("save"),
          class = "btn-success"
        ),
        hr(),
        h3("Import Statement"),
        p("Upload a CSV file (Bank or Credit Card statement) to import expenses."),
        fileInput("file_import", "Choose CSV File", accept = ".csv"),
        uiOutput("import_ui")
      )
    )
  )
)

# Server logic ----------------------------------------------------------------

server <- function(input, output, session) {
  expenses <- reactiveVal(load_expenses())
  budgets <- reactiveVal(load_budgets())
  monthly_income <- reactiveVal(load_monthly_income())
  goals <- reactiveVal(load_goals())
  goal_progress <- reactiveVal(load_goal_progress())

  pending_expense_delete <- reactiveVal(NULL)
  pending_budget_delete <- reactiveVal(NULL)
  staged_expenses <- reactiveVal(NULL)
  staged_render_trigger <- reactiveVal(0) # Trigger for full table re-render

  # Auto-categorization logic -------------------------------------------------

  predict_categories <- function(new_descriptions, history_df) {
    if (nrow(history_df) == 0) {
      return(data.frame(
        Category = rep("", length(new_descriptions)),
        Subcategory = rep("", length(new_descriptions)),
        stringsAsFactors = FALSE
      ))
    }

    # Simple exact substring match based on history
    # For each new description, find historical entries that are substrings of it
    # or where it is a substring of the historical entry.
    # Vote for most common Category/Subcategory pair.

    preds <- lapply(new_descriptions, function(desc) {
      desc_lower <- tolower(desc)

      # exact/substring matches
      matches <- history_df %>%
        filter(
          nzchar(Category), # Has category
          grepl(desc_lower, tolower(Description), fixed = TRUE) |
            grepl(tolower(Description), desc_lower, fixed = TRUE)
        )

      if (nrow(matches) > 0) {
        # Take the most frequent (Category, Subcategory) pair
        best <- matches %>%
          count(Category, Subcategory) %>%
          arrange(desc(n)) %>%
          slice(1)
        return(list(Category = best$Category, Subcategory = best$Subcategory))
      }

      return(list(Category = "", Subcategory = ""))
    })

    bind_rows(preds)
  }

  # Import Logic --------------------------------------------------------------

  observeEvent(input$file_import, {
    req(input$file_import)
    file <- input$file_import

    tryCatch(
      {
        # Read first few lines to detect format
        header_line <- readLines(file$datapath, n = 1)

        raw_df <- NULL
        is_credit_card <- grepl("Status,Date,Description,Debit,Credit,Member Name", header_line, fixed = TRUE)

        if (is_credit_card) {
          # Format A: Credit Card
          # Status,Date,Description,Debit,Credit,Member Name
          dt <- readr::read_csv(
            file$datapath,
            col_types = cols(
              Date = col_character(), # Read as char first to handle formats
              Description = col_character(),
              Debit = col_double(),
              Credit = col_double(),
              .default = col_character()
            )
          )

          # Extract expenses (Debit > 0)
          # Note: User said "only negative" generally, but CC CSV has Debits as positive expenses.
          # We want Expenses.
          raw_df <- dt %>%
            filter(!is.na(Debit) & Debit > 0) %>%
            mutate(
              Amount = Debit,
              Date = mdy(Date),
              Payer = case_when(
                grepl("CARSON", `Member Name`, ignore.case = TRUE) ~ "Carson",
                grepl("CHLOE", `Member Name`, ignore.case = TRUE) ~ "Chloe",
                TRUE ~ "Joint"
              )
            ) %>%
            select(Date, Description, Amount, Payer)
        } else {
          # Format B: Checking / Bank (No Header or Generic)
          # Assuming structure: Col 1 = Date, Col 2 = Amount, Col 5 = Description
          # And Expenses are Negative Amounts.
          dt <- readr::read_csv(
            file$datapath,
            col_names = FALSE,
            col_types = cols(
              X1 = col_character(),
              X2 = col_double(),
              X5 = col_character(),
              .default = col_character()
            )
          )

          # Keep only rows where Amount is negative (Expense)
          raw_df <- dt %>%
            filter(!is.na(X2) & X2 < 0) %>%
            mutate(
              Amount = abs(X2), # Convert to positive for the app
              Date = mdy(X1),
              Description = X5,
              Payer = "Joint"
            ) %>%
            select(Date, Description, Amount, Payer)
        }

        if (nrow(raw_df) == 0) {
          showNotification("No expenses found in file.", type = "warning")
          return()
        }

        # Initialize Staging Data
        # Add ID for tracking
        staged <- raw_df %>%
          mutate(
            id = row_number(),
            Category = "",
            Subcategory = "",
            Duplicate = FALSE
          )

        # Duplicate Detection
        history <- expenses()

        # 1. Internal Deduplication (within the uploaded file)
        # We flag them first, then we will filter at the end of this block
        staged <- staged %>%
          group_by(Date, Description, Amount, Payer) %>%
          mutate(temp_id = row_number()) %>%
          mutate(InternalDuplicate = temp_id > 1) %>%
          ungroup() %>%
          select(-temp_id)

        # 2. Historical Duplicate Detection (against existing data)
        if (nrow(history) > 0) {
          # Check for exact matches on Date & Amount
          # Then check Description similarity
          staged$Duplicate <- vapply(seq_len(nrow(staged)), function(i) {
            row <- staged[i, ]
            candidates <- history %>%
              filter(Date == row$Date, abs(Amount - row$Amount) < 0.01)

            if (nrow(candidates) == 0) {
              return(FALSE)
            }

            # Check description similarity (Levensthein)
            # If any candidate has similarity > threshold, flag as duplicate
            dists <- stringdist::stringdist(tolower(row$Description), tolower(candidates$Description), method = "lv")
            # If description is very short, be strict. If long, allow some diff.
            # Simple heuristic: exact match is best, but let's say "contains" or small diff
            any(dists < 5 | grepl(tolower(row$Description), tolower(candidates$Description), fixed = TRUE))
          }, logical(1))
        }

        # Automatic Removal
        n_initial <- nrow(staged)
        staged <- staged %>% filter(!Duplicate & !InternalDuplicate)
        n_removed <- n_initial - nrow(staged)

        if (n_removed > 0) {
          showNotification(paste("Automatically removed", n_removed, "duplicate entries."), type = "message")
        }

        # Cleanup internal flags
        staged <- staged %>% select(-InternalDuplicate)

        staged_expenses(staged)
        staged_render_trigger(staged_render_trigger() + 1)
      },
      error = function(e) {
        showNotification(paste("Error parsing file:", e$message), type = "error")
      }
    )
  })

  output$import_ui <- renderUI({
    # Only re-render if data is cleared OR a new file is uploaded
    # (Controlled by staged_expenses becoming non-null or null)
    # We isolate the actual data contents so edits don't trigger this UI refresh
    staged_data <- staged_expenses()
    if (is.null(staged_data)) {
      return(NULL)
    }

    tagList(
      h4("Staging Area"),
      p("Review and categorize expenses before importing. Select rows to edit or delete."),
      fluidRow(
        column(
          width = 9,
          div(
            class = "btn-group",
            style = "margin-bottom: 10px;",
            actionButton("auto_categorize", "Auto-Categorize", class = "btn-info"),
            actionButton("delete_selected_staged", "Delete Selected", class = "btn-danger"),
            actionButton("import_selected", "Import Selected", class = "btn-primary"),
            actionButton("clear_staging", "Clear", class = "btn-default")
          ),
          DTOutput("staging_table")
        ),
        column(
          width = 3,
          wellPanel(
            h4("Edit Selected"),
            p(class = "text-muted", "Select rows in the table to edit."),
            selectizeInput(
              "staged_category",
              "Category",
              choices = NULL, # Populated by server
              options = list(placeholder = "Select Category", create = TRUE)
            ),
            selectizeInput(
              "staged_subcategory",
              "Subcategory",
              choices = NULL,
              options = list(placeholder = "Select Subcategory", create = TRUE)
            ),
            selectizeInput(
              "staged_payer",
              "Payer",
              choices = c("Joint", "Carson", "Chloe"),
              options = list(create = TRUE)
            ),
            actionButton("apply_staged_edits", "Apply Changes", class = "btn-success btn-block")
          )
        )
      )
    )
  })

  output$staging_table <- renderDT({
    # Trigger only when file is uploaded or cleared
    staged_render_trigger()

    # Isolate data to prevent edits/deletions from re-triggering this block
    df <- isolate(staged_expenses())
    req(df)

    # Select columns for display
    display_df <- df %>%
      select(Date, Description, Amount, Category, Subcategory, Payer, Duplicate)

    datatable(
      display_df,
      selection = "multiple",
      editable = FALSE, # Disable inline editing
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        stateSave = TRUE,
        scrollX = TRUE,
        rowCallback = JS(
          "function(row, data, index) {",
          "  if(data[7] === true) {", # Duplicate is index 7 (because of rownames)
          "    $('td', row).css('background-color', '#ffe6e6');",
          "    $('td', row).attr('title', 'Potential Duplicate');",
          "  }",
          "}"
        )
      )
    ) %>%
      formatCurrency("Amount") %>%
      formatStyle(
        "Duplicate",
        target = "row",
        backgroundColor = styleEqual(TRUE, "#ffe6e6")
      )
  })

  proxy_staging <- dataTableProxy("staging_table")

  # Sync sidebar inputs with main app categories
  observe({
    req(staged_expenses())
    # Get categories from existing system
    expense_categories <- expenses() %>%
      filter(nzchar(Category)) %>%
      pull(Category)
    budget_categories <- budgets() %>%
      filter(nzchar(Category)) %>%
      pull(Category)

    cats <- sort(unique(c(expense_categories, budget_categories)))

    updateSelectizeInput(session, "staged_category", choices = cats, server = TRUE)
  })

  # Update subcategories based on chosen category in staging
  observe({
    req(input$staged_category)
    cat <- input$staged_category

    expense_subs <- expenses() %>%
      filter(Category == cat, nzchar(Subcategory)) %>%
      pull(Subcategory)
    budget_subs <- budgets() %>%
      filter(Category == cat, nzchar(Subcategory)) %>%
      pull(Subcategory)

    subs <- sort(unique(c(clean_subcategory(expense_subs), clean_subcategory(budget_subs))))
    updateSelectizeInput(session, "staged_subcategory", choices = subs, server = TRUE)
  })

  # Listen to Selection -> Update Sidebar
  observe({
    req(staged_expenses())
    rows <- input$staging_table_rows_selected
    data <- staged_expenses()

    if (is.null(rows) || length(rows) == 0) {
      return()
    }

    # Ensure selected rows exist in data (handles sync lag after deletion)
    rows <- rows[rows <= nrow(data)]
    if (length(rows) == 0) {
      return()
    }

    # If one row selected, populate its values
    # If multiple, populate matching values or clear
    selected_data <- data[rows, ]

    # Check if all selected have same category
    first_cat <- selected_data$Category[1]
    if (!is.na(first_cat) && all(selected_data$Category == first_cat, na.rm = TRUE)) {
      updateSelectizeInput(session, "staged_category", selected = first_cat)
    } else {
      updateSelectizeInput(session, "staged_category", selected = character(0))
    }

    first_sub <- selected_data$Subcategory[1]
    if (!is.na(first_sub) && all(selected_data$Subcategory == first_sub, na.rm = TRUE)) {
      updateSelectizeInput(session, "staged_subcategory", selected = first_sub)
    } else {
      updateSelectizeInput(session, "staged_subcategory", selected = character(0))
    }

    first_payer <- selected_data$Payer[1]
    if (!is.na(first_payer) && all(selected_data$Payer == first_payer, na.rm = TRUE)) {
      updateSelectizeInput(session, "staged_payer", selected = first_payer)
    }
  })

  # Apply Edits
  observeEvent(input$apply_staged_edits, {
    req(staged_expenses())
    rows <- input$staging_table_rows_selected

    if (length(rows) == 0) {
      showNotification("Select rows to apply changes.", type = "warning")
      return()
    }

    current <- staged_expenses()

    cat <- input$staged_category
    sub <- input$staged_subcategory # Can be empty
    payer <- input$staged_payer

    if (nzchar(cat)) current$Category[rows] <- cat

    # If a subcategory is selected, use it. If empty, clearing depends on intent.
    # Let's assume applying logic: if empty, maybe keep existing?
    # But usually applying explicitly means "set to this".
    # Let's allow clearing if it's explicitly cleared in UI (empty string).
    current$Subcategory[rows] <- clean_subcategory(sub)

    if (nzchar(payer)) current$Payer[rows] <- payer

    staged_expenses(current)
    replaceData(proxy_staging, current %>% select(Date, Description, Amount, Category, Subcategory, Payer, Duplicate), resetPaging = FALSE)
    showNotification("Updated selected rows.", type = "message")
  })

  observeEvent(input$auto_categorize, {
    req(staged_expenses())
    current <- staged_expenses()
    history <- expenses()

    preds <- predict_categories(current$Description, history)

    # Only update empty categories
    to_update <- which(!nzchar(current$Category))

    if (length(to_update) > 0) {
      current$Category[to_update] <- preds$Category[to_update]
      current$Subcategory[to_update] <- preds$Subcategory[to_update]
      staged_expenses(current)
      replaceData(proxy_staging, current %>% select(Date, Description, Amount, Category, Subcategory, Payer, Duplicate), resetPaging = FALSE)

      n_filled <- sum(nzchar(preds$Category[to_update]))
      if (n_filled > 0) {
        showNotification(paste("Auto-categorized", n_filled, "items."), type = "message")
      } else {
        showNotification("Auto-categorization found no matches.", type = "warning")
      }
    } else {
      showNotification("No empty categories to update.", type = "message")
    }
  })

  observeEvent(input$remove_duplicates, { # Keep for "Remove Flagged" button if it exists
    req(staged_expenses())
    current <- staged_expenses()
    n_dupes <- sum(current$Duplicate)

    if (n_dupes > 0) {
      current <- current %>% filter(!Duplicate)
      staged_expenses(current)
      replaceData(proxy_staging, current %>% select(Date, Description, Amount, Category, Subcategory, Payer, Duplicate), resetPaging = FALSE)
      showNotification(paste("Removed", n_dupes, "potential duplicates."), type = "message")
    } else {
      showNotification("No duplicates flagged.", type = "message")
    }
  })

  observeEvent(input$delete_selected_staged, {
    req(staged_expenses())
    rows <- input$staging_table_rows_selected

    if (length(rows) == 0) {
      showNotification("Select rows to delete.", type = "warning")
      return()
    }

    current <- staged_expenses()
    current <- current[-rows, ]
    staged_expenses(current)
    replaceData(proxy_staging, current %>% select(Date, Description, Amount, Category, Subcategory, Payer, Duplicate), resetPaging = FALSE)
    showNotification("Deleted selected rows.", type = "message")
  })

  observeEvent(input$clear_staging, {
    staged_expenses(NULL)
    staged_render_trigger(0)
    runjs("document.getElementById('file_import').value = '';")
  })

  observeEvent(input$import_selected, {
    req(staged_expenses())
    rows <- input$staging_table_rows_selected

    if (is.null(rows) || length(rows) == 0) {
      showNotification("Please select rows to import.", type = "warning")
      return()
    }

    current <- staged_expenses()
    to_import <- current[rows, ]

    # Check Categories
    if (any(!nzchar(to_import$Category))) {
      showNotification("Warning: Some selected items have no category.", type = "warning")
    }

    # STRICT DUPLICATE CHECK against main expenses
    existing <- expenses()

    # Strict matching on Date, Amount, Description
    duplicates <- inner_join(
      to_import,
      existing,
      by = c("Date", "Amount", "Description")
    )

    if (nrow(duplicates) > 0) {
      unique_imports <- anti_join(
        to_import,
        existing,
        by = c("Date", "Amount", "Description")
      )

      skipped_count <- nrow(to_import) - nrow(unique_imports)
      if (skipped_count > 0) {
        showNotification(paste("Skipped", skipped_count, "items that already exist in expenses."), type = "warning")
      }

      final_import <- unique_imports
    } else {
      final_import <- to_import
    }

    if (nrow(final_import) > 0) {
      new_entries <- final_import %>%
        transmute(
          Date = Date,
          Description = Description,
          Category = Category,
          Subcategory = Subcategory,
          Amount = Amount,
          Payer = Payer
        )

      updated <- bind_rows(expenses(), new_entries) %>% arrange(desc(Date))
      expenses(updated)
      write_expenses(updated)

      showNotification(paste("Imported", nrow(new_entries), "items."), type = "message")
    }

    # Remove the originally selected rows from staging
    remaining <- current[-rows, ]
    if (nrow(remaining) == 0) {
      staged_expenses(NULL)
    } else {
      staged_expenses(remaining)
    }
  })

  pending_expense_delete <- reactiveVal(NULL)
  pending_budget_delete <- reactiveVal(NULL)

  observeEvent(input$backup_data, {
    # Define backup directory (relative to the app folder)
    # The app is running in 'app/', so '..' goes to project root
    backup_dir <- normalizePath(file.path("..", "backups"), mustWork = FALSE)

    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d")
    data_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

    if (length(data_files) == 0) {
      showNotification("No data files found to backup.", type = "warning")
      return()
    }

    success_count <- 0
    for (file in data_files) {
      base_name <- tools::file_path_sans_ext(basename(file))
      ext <- tools::file_ext(file)
      new_name <- paste0(base_name, "_", timestamp, "-backup.", ext)
      target_path <- file.path(backup_dir, new_name)

      if (file.copy(file, target_path, overwrite = TRUE)) {
        success_count <- success_count + 1
      }
    }

    if (success_count > 0) {
      showNotification(
        paste("Successfully backed up", success_count, "files to 'backups' folder."),
        type = "message"
      )
    } else {
      showNotification("Backup failed.", type = "error")
    }
  })

  observeEvent(
    TRUE,
    {
      updateNumericInput(session, "income", value = monthly_income())
    },
    once = TRUE
  )

  observe({
    expense_categories <- expenses() %>%
      filter(nzchar(Category)) %>%
      pull(Category)

    budget_categories <- budgets() %>%
      filter(nzchar(Category)) %>%
      pull(Category)

    categories <- sort(unique(c(expense_categories, budget_categories)))

    if (!is.null(input$expense_category) && nzchar(input$expense_category)) {
      categories <- unique(c(categories, input$expense_category))
    }

    if (!is.null(input$budget_category) && nzchar(input$budget_category)) {
      categories <- unique(c(categories, input$budget_category))
    }
    updateSelectizeInput(
      session,
      "expense_category",
      choices = categories,
      selected = input$expense_category,
      server = FALSE
    )

    updateSelectizeInput(
      session,
      "budget_category",
      choices = categories,
      selected = input$budget_category,
      server = FALSE
    )
  })

  observe({
    payers <- expenses() %>%
      filter(nzchar(Payer)) %>%
      pull(Payer)

    payers <- sort(unique(c(default_payers, payers)))

    if (!is.null(input$expense_payer) && nzchar(input$expense_payer)) {
      payers <- unique(c(payers, input$expense_payer))
    }

    updateSelectizeInput(
      session,
      "expense_payer",
      choices = payers,
      selected = input$expense_payer,
      server = FALSE
    )
  })

  observe({
    dates <- expenses()$Date

    # Always include current month in options, even if no expenses yet
    current_month <- floor_date(Sys.Date(), "month")

    if (length(dates) == 0) {
      months <- current_month
    } else {
      months <- sort(unique(c(floor_date(dates, "month"), current_month)), decreasing = TRUE)
    }

    month_names <- format(months, "%B %Y")
    month_values <- format(months, "%Y-%m-%d")
    choices <- setNames(month_values, month_names)
    choices <- c("All time" = "all", choices)

    # Default to current month if "all" is selected (initial state) or if current selection is invalid
    selected <- input$report_month
    current_month_val <- format(current_month, "%Y-%m-%d")

    if (is.null(selected) || selected == "all") {
      selected <- current_month_val
    }

    updateSelectInput(
      session,
      "report_month",
      choices = choices,
      selected = selected
    )
  })

  observe({
    category <- input$expense_category

    if (is.null(category) || !nzchar(category)) {
      updateSelectizeInput(
        session,
        "expense_subcategory",
        choices = "",
        selected = "",
        server = FALSE
      )
      return()
    }

    expense_subs <- expenses() %>%
      filter(Category == category, nzchar(Subcategory)) %>%
      pull(Subcategory)

    budget_subs <- budgets() %>%
      filter(Category == category, nzchar(Subcategory)) %>%
      pull(Subcategory)

    subchoices <- sort(unique(c(
      clean_subcategory(expense_subs),
      clean_subcategory(budget_subs)
    )))

    current <- clean_subcategory(input$expense_subcategory)
    choices <- unique(c("", subchoices))
    if (nzchar(current)) {
      choices <- unique(c(choices, current))
    }

    named_choices <- structure(choices, names = choices)

    updateSelectizeInput(
      session,
      "expense_subcategory",
      choices = named_choices,
      selected = if (nzchar(current)) current else "",
      server = FALSE
    )
  })

  observe({
    category <- input$budget_category

    if (is.null(category) || !nzchar(category)) {
      updateSelectizeInput(
        session,
        "budget_subcategory",
        choices = "",
        selected = "",
        server = FALSE
      )
      return()
    }

    subchoices <- budgets() %>%
      filter(Category == category, nzchar(Subcategory)) %>%
      pull(Subcategory) %>%
      clean_subcategory() %>%
      sort()

    current <- clean_subcategory(input$budget_subcategory)
    choices <- unique(c("", subchoices))
    if (nzchar(current)) {
      choices <- unique(c(choices, current))
    }

    named_choices <- structure(choices, names = choices)

    updateSelectizeInput(
      session,
      "budget_subcategory",
      choices = named_choices,
      selected = if (nzchar(current)) current else "",
      server = FALSE
    )
  })

  observeEvent(input$add_expense, {
    description <- trimws(input$expense_description)
    category <- trimws(input$expense_category)
    subcategory <- trimws(input$expense_subcategory)
    payer <- trimws(input$expense_payer)

    validate(
      need(
        !is.null(input$expense_date) && !is.na(input$expense_date),
        "Please supply a date."
      ),
      need(nzchar(description), "Describe the expense."),
      need(nzchar(category), "Choose a category."),
      need(
        !is.null(input$expense_amount) &&
          !is.na(input$expense_amount) &&
          input$expense_amount > 0,
        "Enter a positive amount."
      )
    )

    entry <- tibble::tibble(
      Date = as.Date(input$expense_date),
      Description = description,
      Category = category,
      Subcategory = clean_subcategory(subcategory),
      Amount = as.numeric(input$expense_amount),
      Payer = payer
    )

    updated <- bind_rows(expenses(), entry) %>% arrange(desc(Date))
    expenses(updated)
    write_expenses(updated)

    updateTextInput(session, "expense_description", value = "")
    updateNumericInput(session, "expense_amount", value = NA)
    updateSelectizeInput(
      session,
      "expense_subcategory",
      selected = NULL,
      server = FALSE
    )
    showNotification("Expense added.", type = "message")
  })

  observeEvent(input$delete_expense, {
    selected <- input$expense_table_rows_selected
    if (length(selected) == 0) {
      showNotification("Select an expense to delete.", type = "warning")
      return()
    }

    current <- expenses()
    if (nrow(current) == 0) {
      showNotification("No expenses to delete.", type = "warning")
      return()
    }

    valid <- selected[selected >= 1 & selected <= nrow(current)]
    if (length(valid) == 0) {
      showNotification(
        "Selected expense is no longer available.",
        type = "error"
      )
      return()
    }

    row_idx <- valid[1]
    record <- current[row_idx, , drop = FALSE]
    pending_expense_delete(list(row_data = record))

    showModal(modalDialog(
      title = "Delete expense",
      easyClose = FALSE,
      size = "m",
      tags$p("Are you sure you want to delete this expense?"),
      tags$ul(
        tags$li(strong("Date:"), format(record$Date)),
        tags$li(strong("Description:"), record$Description),
        tags$li(
          strong("Category:"),
          paste(
            record$Category,
            format_subcategory(record$Subcategory),
            sep = " › "
          )
        ),
        tags$li(strong("Amount:"), scales::dollar(record$Amount)),
        tags$li(
          strong("Payer:"),
          ifelse(nzchar(record$Payer), record$Payer, "--")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_delete_expense",
          "Delete",
          class = "btn btn-danger"
        )
      )
    ))
  })

  observeEvent(input$add_budget, {
    category <- trimws(input$budget_category)
    subcategory <- clean_subcategory(input$budget_subcategory)
    frequency <- input$budget_frequency
    effective_date <- as.Date(input$budget_start)

    validate(
      need(!is.null(effective_date) && !is.na(effective_date), "Provide an effective date."),
      need(nzchar(category), "Provide a category."),
      need(
        !is.null(input$budget_limit) &&
          !is.na(input$budget_limit) &&
          input$budget_limit >= 0,
        "Enter a non-negative limit."
      )
    )

    new_budget <- tibble::tibble(
      Category = category,
      Subcategory = subcategory,
      Limit = as.numeric(input$budget_limit),
      Frequency = frequency,
      EffectiveDate = effective_date,
      ConclusionDate = as.Date(NA)
    )

    current <- budgets()

    # Remove any existing entry with the exact same category, subcategory, and effective date
    # as we want to overwrite it if it exists.
    # Otherwise, we just add it and recalculate conclusions.
    updated <- current %>%
      filter(!(tolower(Category) == tolower(category) &
        tolower(clean_subcategory(Subcategory)) == tolower(subcategory) &
        EffectiveDate == effective_date)) %>%
      bind_rows(new_budget) %>%
      calculate_budget_conclusions()

    budgets(updated)
    write_budgets(updated)

    updateSelectizeInput(
      session,
      "budget_subcategory",
      selected = NULL,
      server = FALSE
    )
    updateNumericInput(session, "budget_limit", value = NA)
    updateSelectizeInput(session, "budget_frequency", selected = "Monthly")
    showNotification("Budget saved.", type = "message")
  })

  observeEvent(input$update_budget_btn, {
    # Check both current and future tables for selection
    selected_current <- input$budget_table_rows_selected
    selected_future <- input$future_budget_table_rows_selected

    current_data <- budgets() %>%
      filter(EffectiveDate <= Sys.Date() & (is.na(ConclusionDate) | ConclusionDate >= Sys.Date()))
    future_data <- budgets() %>%
      filter(EffectiveDate > Sys.Date())

    record <- NULL
    if (length(selected_current) > 0) {
      record <- current_data[selected_current[1], ]
    } else if (length(selected_future) > 0) {
      record <- future_data[selected_future[1], ]
    }

    if (is.null(record)) {
      showNotification("Select a budget line from either table to update.", type = "warning")
      return()
    }

    # Pre-fill the form
    updateSelectizeInput(session, "budget_category", selected = record$Category)
    updateSelectizeInput(session, "budget_subcategory", selected = clean_subcategory(record$Subcategory))
    updateNumericInput(session, "budget_limit", value = record$Limit)
    updateSelectizeInput(session, "budget_frequency", selected = record$Frequency)
    # Default effective date to next month if updating a current one,
    # or keep its own if it's already in the future.
    eff_date <- if (record$EffectiveDate <= Sys.Date()) {
      floor_date(Sys.Date() + months(1), "month")
    } else {
      record$EffectiveDate
    }
    updateDateInput(session, "budget_start", value = eff_date)

    showNotification("Budget details loaded into form. Set the effective date and save.", type = "message")
  })

  observeEvent(input$delete_budget, {
    selected_current <- input$budget_table_rows_selected
    selected_future <- input$future_budget_table_rows_selected

    current_data <- budgets() %>%
      filter(EffectiveDate <= Sys.Date() & (is.na(ConclusionDate) | ConclusionDate >= Sys.Date()))
    future_data <- budgets() %>%
      filter(EffectiveDate > Sys.Date())

    record <- NULL
    if (length(selected_current) > 0) {
      record <- current_data[selected_current[1], ]
    } else if (length(selected_future) > 0) {
      record <- future_data[selected_future[1], ]
    }

    if (is.null(record)) {
      showNotification("Select a budget to delete from either table.", type = "warning")
      return()
    }

    record <- as.list(record)
    pending_budget_delete(list(row_data = record))

    showModal(modalDialog(
      title = "Remove Budget",
      easyClose = FALSE,
      size = "m",
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        "How would you like to remove this budget?"
      ),
      radioButtons(
        "delete_mode",
        label = NULL,
        choices = c(
          "Stop budgeting (Preserve history)" = "archive",
          "Delete record (Correction)" = "delete"
        ),
        selected = "archive"
      ),
      tags$div(
        style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
        tags$p(strong("Selected Item:")),
        tags$ul(
          tags$li(strong("Category:"), record$Category),
          tags$li(strong("Subcategory:"), format_subcategory(record$Subcategory)),
          tags$li(strong("Limit:"), scales::dollar(record$Limit)),
          tags$li(strong("Frequency:"), record$Frequency),
          tags$li(strong("Effective Date:"), format(record$EffectiveDate))
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_delete_budget",
          "Confirm",
          class = "btn btn-primary"
        )
      )
    ))
  })

  observeEvent(input$confirm_delete_expense, {
    info <- pending_expense_delete()
    pending_expense_delete(NULL)
    removeModal()

    if (is.null(info)) {
      return()
    }

    current <- expenses()
    if (nrow(current) == 0) {
      showNotification(
        "Selected expense is no longer available.",
        type = "error"
      )
      return()
    }

    record <- info$row_data
    match_idx <- which(
      current$Date == record$Date &
        current$Description == record$Description &
        current$Category == record$Category &
        current$Subcategory == record$Subcategory &
        current$Amount == record$Amount &
        current$Payer == record$Payer
    )

    if (length(match_idx) == 0) {
      showNotification(
        "Selected expense is no longer available.",
        type = "error"
      )
      return()
    }

    updated <- current[-match_idx[1], , drop = FALSE]
    expenses(updated)
    write_expenses(updated)
    showNotification("Expense deleted.", type = "message")
  })

  observeEvent(input$confirm_delete_budget, {
    info <- pending_budget_delete()
    pending_budget_delete(NULL)
    removeModal()

    if (is.null(info)) {
      return()
    }

    current <- budgets()
    # Re-verify the data is still valid
    if (nrow(current) == 0) {
      return()
    }

    record <- info$row_data

    # Mode Handling
    mode <- input$delete_mode
    if (is.null(mode)) mode <- "archive" # Default safe fallback

    if (mode == "archive") {
      # ARCHIVE: Create a new 0-limit entry effective next month (or today if future item being archived)
      new_date <- if (as.Date(record$EffectiveDate) > Sys.Date()) {
        as.Date(record$EffectiveDate)
      } else {
        floor_date(Sys.Date() + months(1), "month")
      }

      new_entry <- tibble::tibble(
        Category = record$Category,
        Subcategory = record$Subcategory,
        Limit = 0,
        Frequency = record$Frequency,
        EffectiveDate = new_date,
        ConclusionDate = as.Date(NA)
      )

      updated <- current %>%
        filter(!(tolower(Category) == tolower(record$Category) &
          tolower(clean_subcategory(Subcategory)) == tolower(clean_subcategory(record$Subcategory)) &
          EffectiveDate == new_date)) %>%
        bind_rows(new_entry) %>%
        calculate_budget_conclusions()

      budgets(updated)
      write_budgets(updated)
      showNotification("Budget stopped effective next month.", type = "message")
    } else {
      # DELETE: Physically remove the record
      updated <- current %>%
        filter(!(tolower(Category) == tolower(record$Category) &
          tolower(clean_subcategory(Subcategory)) == tolower(clean_subcategory(record$Subcategory)) &
          EffectiveDate == as.Date(record$EffectiveDate))) %>%
        calculate_budget_conclusions()

      budgets(updated)
      write_budgets(updated)
      showNotification("Budget record deleted.", type = "message")
    }
  })

  observeEvent(input$set_income, {
    validate(
      need(
        !is.null(input$income) && !is.na(input$income) && input$income >= 0,
        "Enter a non-negative income."
      )
    )
    amount <- as.numeric(input$income)
    monthly_income(amount)
    write_monthly_income(amount)
    showNotification("Income updated.", type = "message")
  })

  # Goals Logic ---------------------------------------------------------------

  progress_bar <- function(value, label = "") {
    tags$div(
      class = "progress",
      style = "margin-bottom: 5px;",
      tags$div(
        class = "progress-bar progress-bar-success",
        role = "progressbar",
        `aria-valuenow` = value,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        style = paste0("width: ", value, "%;"),
        label
      )
    )
  }

  get_goal_monthly_saving <- function(g) {
    # Months between CreatedDate and TargetMonth
    total_months <- interval(floor_date(g$CreatedDate, "month"), g$TargetMonth) %/% months(1)
    if (total_months <= 0) total_months <- 1
    g$TargetAmount / total_months
  }

  observeEvent(input$add_goal, {
    name <- trimws(input$goal_name)
    target <- as.numeric(input$goal_target)
    month <- as.Date(input$goal_target_month)

    validate(
      need(nzchar(name), "Provide a goal name."),
      need(!is.na(target) && target > 0, "Enter a positive target amount."),
      need(!is.na(month) && month > Sys.Date(), "Target month must be in the future.")
    )

    new_goal <- tibble::tibble(
      Goal = name,
      TargetAmount = target,
      TargetMonth = floor_date(month, "month"),
      CreatedDate = Sys.Date()
    )

    current <- goals()
    if (name %in% current$Goal) {
      current <- current %>% filter(Goal != name)
    }

    updated <- bind_rows(current, new_goal)
    goals(updated)
    write_goals(updated)

    updateTextInput(session, "goal_name", value = "")
    updateNumericInput(session, "goal_target", value = NA)
    showNotification("Goal saved.", type = "message")
  })

  output$goals_list_ui <- renderUI({
    current_goals <- goals()
    if (nrow(current_goals) == 0) {
      return(tags$p("No goals added yet."))
    }

    current_month <- floor_date(Sys.Date(), "month")
    progress_data <- goal_progress()

    goal_elements <- lapply(seq_len(nrow(current_goals)), function(i) {
      g <- current_goals[i, ]

      monthly_saving <- get_goal_monthly_saving(g)

      # Percentage based on months saved
      months_saved <- progress_data %>%
        filter(Goal == g$Goal, Saved == TRUE) %>%
        nrow()

      total_months_needed <- interval(floor_date(g$CreatedDate, "month"), g$TargetMonth) %/% months(1)
      if (total_months_needed <= 0) total_months_needed <- 1

      percent <- min(100, round((months_saved / total_months_needed) * 100))

      is_saved_this_month <- progress_data %>%
        filter(Goal == g$Goal, Month == current_month) %>%
        pull(Saved)
      if (length(is_saved_this_month) == 0) is_saved_this_month <- FALSE

      wellPanel(
        style = "padding: 10px; margin-bottom: 10px;",
        fluidRow(
          column(8, strong(g$Goal, style = "font-size: 1.2em;")),
          column(4,
            align = "right",
            actionButton(paste0("del_goal_", i), "",
              icon = icon("trash"), class = "btn-danger btn-xs",
              onclick = sprintf("Shiny.setInputValue('delete_goal_id', %d, {priority: 'event'})", i)
            )
          )
        ),
        p(paste0("Target: ", dollar(g$TargetAmount), " by ", format(g$TargetMonth, "%B %Y")), style = "margin-bottom: 5px;"),
        p(paste0("Monthly saving required: ", dollar(monthly_saving)), style = "margin-bottom: 10px;"),
        progress_bar(percent, label = paste0(percent, "%")),
        checkboxInput(paste0("save_goal_", i), "Money saved for this month", value = is_saved_this_month)
      )
    })

    do.call(tagList, goal_elements)
  })

  observeEvent(input$delete_goal_id, {
    idx <- input$delete_goal_id
    current <- goals()
    if (idx > 0 && idx <= nrow(current)) {
      goal_name <- current$Goal[idx]
      updated_goals <- current[-idx, ]
      goals(updated_goals)
      write_goals(updated_goals)

      updated_prog <- goal_progress() %>% filter(Goal != goal_name)
      goal_progress(updated_prog)
      write_goal_progress(updated_prog)

      showNotification("Goal deleted.", type = "warning")
    }
  })

  observe({
    current_goals <- goals()
    if (nrow(current_goals) == 0) {
      return()
    }

    current_month <- floor_date(Sys.Date(), "month")
    prog <- goal_progress()
    changed <- FALSE

    for (i in seq_len(nrow(current_goals))) {
      input_id <- paste0("save_goal_", i)
      if (!is.null(input[[input_id]])) {
        val <- input[[input_id]]
        goal_name <- current_goals$Goal[i]

        existing_idx <- which(prog$Goal == goal_name & prog$Month == current_month)
        is_saved <- if (length(existing_idx) > 0) prog$Saved[existing_idx] else FALSE

        if (val != is_saved) {
          if (length(existing_idx) > 0) {
            prog$Saved[existing_idx] <- val
          } else {
            prog <- bind_rows(prog, tibble(Goal = goal_name, Month = current_month, Saved = val))
          }
          changed <- TRUE
        }
      }
    }

    if (changed) {
      goal_progress(prog)
      write_goal_progress(prog)
    }
  })

  output$goals_monthly_summary <- renderUI({
    current_goals <- goals()
    if (nrow(current_goals) == 0) {
      return(tags$p("No active goals."))
    }

    total_monthly <- sum(vapply(seq_len(nrow(current_goals)), function(i) {
      get_goal_monthly_saving(current_goals[i, ])
    }, numeric(1)))

    tags$div(
      p(strong("Total Goal Savings/Month: "), dollar(total_monthly))
    )
  })

  expense_totals <- reactive({
    df <- expenses()
    tibble::tibble(
      Total = sum(df$Amount, na.rm = TRUE),
      Count = nrow(df)
    )
  })

  output$expense_totals <- renderText({
    totals <- expense_totals()
    paste0(
      "Logged expenses: ",
      totals$Count,
      " | Total spent: ",
      dollar(totals$Total)
    )
  })

  output$expense_table <- renderDT({
    data <- format_expense_table_data(expenses())

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20)),
      selection = "single",
      editable = list(target = "cell")
    ) %>%
      formatCurrency(
        "Amount",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      )
  })

  output$budget_table <- renderDT({
    data <- budgets() %>%
      filter(EffectiveDate <= Sys.Date() & (is.na(ConclusionDate) | ConclusionDate >= Sys.Date())) %>%
      format_budget_table_data()

    validate(need(nrow(data) > 0, "No current budgets found."))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20)),
      selection = "single",
      editable = list(target = "cell")
    ) %>%
      formatCurrency("Limit")
  })

  output$future_budget_table <- renderDT({
    data <- budgets() %>%
      filter(EffectiveDate > Sys.Date()) %>%
      format_budget_table_data()

    validate(need(nrow(data) > 0, "No future budgets scheduled."))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20)),
      selection = "single"
    ) %>%
      formatCurrency("Limit")
  })

  # Reactive expression to calculate suggested budgets
  suggested_budgets <- reactive({
    curr_budgets <- budgets() %>%
      filter(
        EffectiveDate <= Sys.Date() &
          (is.na(ConclusionDate) | ConclusionDate >= Sys.Date()) &
          Frequency == "Monthly"
      )

    if (nrow(curr_budgets) == 0) {
      return(NULL)
    }

    exp_data <- expenses()

    # Calculate current month's spending
    eval_month_start <- floor_date(Sys.Date(), "month")
    eval_month_end <- ceiling_date(eval_month_start, "month") - days(1)

    suggestions <- list()
    for (i in seq_len(nrow(curr_budgets))) {
      b <- curr_budgets[i, ]
      cat_name <- b$Category
      subcat_name <- b$Subcategory
      limit_val <- b$Limit

      # Filter expenses for current month
      eval_month_exp <- exp_data %>%
        filter(
          Date >= eval_month_start,
          Date <= eval_month_end,
          Category == cat_name,
          (nzchar(subcat_name) == FALSE | Subcategory == subcat_name)
        )

      eval_month_total <- sum(eval_month_exp$Amount, na.rm = TRUE)

      # Check condition: did current month's spending differ from limit by > $50?
      if (abs(eval_month_total - limit_val) > 50) {
        # Get historical data to calculate WMA
        hist_exp <- exp_data %>%
          filter(
            Date >= floor_date(Sys.Date() - months(11), "month"),
            Date <= eval_month_end,
            Category == cat_name,
            (nzchar(subcat_name) == FALSE | Subcategory == subcat_name)
          ) %>%
          mutate(MonthGroup = floor_date(Date, "month")) %>%
          group_by(MonthGroup) %>%
          summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop")

        all_months <- tibble(MonthGroup = seq(floor_date(Sys.Date() - months(11), "month"), eval_month_start, by = "1 month"))
        ts_data <- all_months %>%
          left_join(hist_exp, by = "MonthGroup") %>%
          mutate(Total = replace_na(Total, 0)) %>%
          arrange(MonthGroup)

        # We need the 3 most recent months: index 1 is oldest, index 3 is newest.
        recent_3 <- tail(ts_data$Total, 3)
        hasty_wma <- sum(recent_3 * c(0.1, 0.3, 0.6))
        conservative_wma <- sum(recent_3 * c(0.2, 0.4, 0.4))

        suggestions[[length(suggestions) + 1]] <- tibble(
          Category = cat_name,
          Subcategory = subcat_name,
          `Current Limit` = limit_val,
          `Hasty Limit` = round(hasty_wma),
          `Conservative Limit` = round(conservative_wma),
          `Current Month Spent` = eval_month_total
        )
      }
    }

    if (length(suggestions) > 0) {
      bind_rows(suggestions)
    } else {
      NULL
    }
  })

  output$suggested_budget_table <- renderDT({
    data <- suggested_budgets()

    validate(need(!is.null(data) && nrow(data) > 0, "No suggestions available right now."))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 5, dom = "tip"),
      selection = "multiple"
    ) %>%
      formatCurrency(c("Current Limit", "Hasty Limit", "Conservative Limit", "Current Month Spent"))
  })

  apply_suggestions <- function(selected_rows, limit_col) {
    data <- suggested_budgets()
    req(data)

    suggestions_to_apply <- data[selected_rows, ]
    current_budgets <- budgets()

    for (i in seq_len(nrow(suggestions_to_apply))) {
      row_item <- suggestions_to_apply[i, ]

      new_entry <- tibble(
        Category = row_item$Category,
        Subcategory = row_item$Subcategory,
        Limit = row_item[[limit_col]],
        Frequency = "Monthly",
        EffectiveDate = floor_date(Sys.Date() + months(1), "month"),
        ConclusionDate = as.Date(NA)
      )

      current_budgets <- current_budgets %>%
        filter(!(
          Category == row_item$Category &
            Subcategory == row_item$Subcategory &
            EffectiveDate == new_entry$EffectiveDate
        ))

      current_budgets <- bind_rows(current_budgets, new_entry)
    }

    current_budgets <- calculate_budget_conclusions(current_budgets)
    write_budgets(current_budgets)
    budgets(current_budgets)

    showNotification(paste("Applied", nrow(suggestions_to_apply), "budget suggestions using", limit_col, "."), type = "message")
  }

  observeEvent(input$apply_hasty_btn, {
    req(input$suggested_budget_table_rows_selected)
    apply_suggestions(input$suggested_budget_table_rows_selected, "Hasty Limit")
  })

  observeEvent(input$apply_conservative_btn, {
    req(input$suggested_budget_table_rows_selected)
    apply_suggestions(input$suggested_budget_table_rows_selected, "Conservative Limit")
  })

  expense_proxy <- dataTableProxy("expense_table")
  budget_proxy <- dataTableProxy("budget_table")

  observeEvent(input$expense_table_cell_edit, {
    info <- input$expense_table_cell_edit
    df <- expenses()

    if (is.null(info$row) || is.null(info$col)) {
      DT::replaceData(
        expense_proxy,
        format_expense_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    row_idx <- as.integer(info$row)
    rows_all <- input$expense_table_rows_all
    if (!is.null(rows_all)) {
      if (row_idx < 1 || row_idx > length(rows_all)) {
        DT::replaceData(
          expense_proxy,
          format_expense_table_data(df),
          resetPaging = FALSE,
          rownames = FALSE
        )
        return()
      }
      row_idx <- as.integer(rows_all[row_idx])
    }

    if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(df)) {
      DT::replaceData(
        expense_proxy,
        format_expense_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    col_idx <- as.integer(info$col)
    if (is.na(col_idx) || col_idx < 1 || col_idx > ncol(df)) {
      DT::replaceData(
        expense_proxy,
        format_expense_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    column <- names(df)[col_idx]
    value <- info$value

    parse_amount <- function(x) {
      as.numeric(gsub("[^0-9.-]", "", x))
    }

    updated_value <- switch(column,
      Date = {
        parsed <- tryCatch(
          as.Date(value),
          error = function(e) NA
        )
        if (is.na(parsed)) {
          showNotification("Enter a valid date (YYYY-MM-DD).", type = "error")
          return(DT::replaceData(
            expense_proxy,
            format_expense_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        parsed
      },
      Amount = {
        parsed <- parse_amount(value)
        if (is.na(parsed) || parsed <= 0) {
          showNotification("Enter a positive amount.", type = "error")
          return(DT::replaceData(
            expense_proxy,
            format_expense_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        parsed
      },
      Subcategory = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned) || identical(cleaned, "(Unspecified)")) {
          ""
        } else {
          clean_subcategory(cleaned)
        }
      },
      Category = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned)) {
          showNotification("Category cannot be empty.", type = "error")
          return(DT::replaceData(
            expense_proxy,
            format_expense_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        cleaned
      },
      Description = trimws(value),
      Payer = trimws(value),
      value
    )

    if (is.null(updated_value)) {
      return()
    }

    df[row_idx, column] <- updated_value
    df <- df %>% arrange(desc(Date))
    expenses(df)
    write_expenses(df)
    DT::replaceData(
      expense_proxy,
      format_expense_table_data(df),
      resetPaging = FALSE,
      rownames = FALSE
    )
  })

  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df <- budgets()

    current_data <- df %>%
      filter(EffectiveDate <= Sys.Date() & (is.na(ConclusionDate) | ConclusionDate >= Sys.Date()))

    refresh_proxy <- function() {
      DT::replaceData(
        budget_proxy,
        format_budget_table_data(current_data),
        resetPaging = FALSE,
        rownames = FALSE
      )
    }

    if (is.null(info$row) || is.null(info$col)) {
      refresh_proxy()
      return()
    }

    row_idx <- as.integer(info$row)
    rows_all <- input$budget_table_rows_all
    if (!is.null(rows_all)) {
      if (row_idx < 1 || row_idx > length(rows_all)) {
        refresh_proxy()
        return()
      }
      row_idx <- as.integer(rows_all[row_idx])
    }

    if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(current_data)) {
      refresh_proxy()
      return()
    }

    col_idx <- as.integer(info$col)
    if (is.na(col_idx) || col_idx < 1 || col_idx > ncol(current_data)) {
      refresh_proxy()
      return()
    }

    record <- current_data[row_idx, ]
    original_idx <- which(df$Category == record$Category &
      df$Subcategory == record$Subcategory &
      df$EffectiveDate == record$EffectiveDate)

    if (length(original_idx) == 0) {
      refresh_proxy()
      return()
    }
    original_idx <- original_idx[1]

    column <- names(current_data)[col_idx]
    value <- info$value

    parse_amount <- function(x) {
      as.numeric(gsub("[^0-9.-]", "", x))
    }

    updated_value <- switch(column,
      Limit = {
        parsed <- parse_amount(value)
        if (is.na(parsed) || parsed < 0) {
          showNotification("Enter a non-negative limit.", type = "error")
          return(refresh_proxy())
        }
        parsed
      },
      Category = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned)) {
          showNotification("Category cannot be empty.", type = "error")
          return(refresh_proxy())
        }
        cleaned
      },
      Subcategory = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned) || identical(cleaned, "(Unspecified)")) {
          ""
        } else {
          clean_subcategory(cleaned)
        }
      },
      value
    )

    if (is.null(updated_value)) {
      return()
    }

    df[original_idx, column] <- updated_value
    df <- df %>% arrange(Category, Subcategory, desc(EffectiveDate))
    budgets(df)
    write_budgets(df)

    # Recreate current_data with the updated df
    current_data_updated <- df %>%
      filter(EffectiveDate <= Sys.Date() & (is.na(ConclusionDate) | ConclusionDate >= Sys.Date()))

    DT::replaceData(
      budget_proxy,
      format_budget_table_data(current_data_updated),
      resetPaging = FALSE,
      rownames = FALSE
    )
  })

  output$income_summary <- renderUI({
    income <- monthly_income()
    budget_total <- budgets() %>%
      filter(EffectiveDate <= Sys.Date() & (is.na(ConclusionDate) | ConclusionDate >= Sys.Date())) %>%
      mutate(MonthlyLimit = get_monthly_limit(Limit, Frequency)) %>%
      summarise(Total = sum(MonthlyLimit, na.rm = TRUE)) %>%
      pull(Total)

    goal_monthly_total <- sum(vapply(seq_len(nrow(goals())), function(i) {
      get_goal_monthly_saving(goals()[i, ])
    }, numeric(1)))

    remaining <- if (is.na(income)) NA_real_ else income - budget_total - goal_monthly_total

    tags$div(
      tags$p(
        strong("Monthly income:"),
        if (is.na(income)) "Not set" else dollar(income)
      ),
      tags$p(
        strong("Budgeted total:"),
        dollar(budget_total)
      ),
      tags$p(
        strong("Goal savings:"),
        dollar(goal_monthly_total)
      ),
      tags$p(
        strong("Unallocated:"),
        if (is.na(remaining)) "--" else dollar(remaining)
      )
    )
  })


  output$trend_category_filter <- renderUI({
    if (!identical(input$spending_view, "category")) {
      return(NULL)
    }

    categories <- expenses() %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)")
      ) %>%
      pull(Category) %>%
      unique() %>%
      sort()

    if (length(categories) == 0) {
      return(tags$p("Add expenses to choose categories."))
    }

    selected <- input$trend_categories
    selected <- selected[selected %in% categories]
    if (length(selected) == 0) {
      selected <- categories
    }

    selectizeInput(
      "trend_categories",
      "Categories",
      choices = categories,
      selected = selected,
      multiple = TRUE,
      options = list(
        placeholder = "Filter categories",
        plugins = list("remove_button")
      )
    )
  })

  output$spending_trend <- renderPlotly({
    df <- expenses()
    validate(need(nrow(df) > 0, "Add expenses to see spending trends."))

    df <- df %>%
      filter(!is.na(Date)) %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)"),
        Amount = replace_na(Amount, 0)
      )

    validate(need(
      nrow(df) > 0,
      "Add expenses with dates to see spending trends."
    ))

    unit <- if (identical(input$spending_period, "week")) "week" else "month"
    df <- df %>%
      mutate(
        Period = if (unit == "week") {
          floor_date(Date, unit = "week", week_start = 1)
        } else {
          floor_date(Date, unit = "month")
        }
      )

    if (!identical(input$spending_view, "category")) {
      summary <- df %>%
        group_by(Period) %>%
        summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        arrange(Period)

      validate(need(nrow(summary) > 0, "Add expenses to see spending trends."))

      plot_ly(
        summary,
        x = ~Period,
        y = ~Total,
        type = "scatter",
        mode = "lines+markers",
        hovertemplate = paste0(
          "%{x|%b %d, %Y}<br>Total: $%{y:,.2f}<extra></extra>"
        ),
        name = "Total"
      ) %>%
        layout(
          xaxis = list(title = if (unit == "week") "Week" else "Month"),
          yaxis = list(
            title = "Spending",
            tickprefix = "$",
            separatethousands = TRUE
          ),
          legend = list(orientation = "h", x = 0, y = -0.2),
          title = "Spending over time"
        )
    } else {
      selected <- input$trend_categories
      if (!is.null(selected) && length(selected) > 0) {
        df <- df %>% filter(Category %in% selected)
      }

      summary <- df %>%
        group_by(Period, Category) %>%
        summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        arrange(Period)

      validate(need(
        nrow(summary) > 0,
        "Adjust filters to see spending trends."
      ))

      plt <- plot_ly()
      categories <- unique(summary$Category)
      for (cat in categories) {
        cat_data <- summary %>% filter(Category == cat)
        plt <- plt %>%
          add_trace(
            data = cat_data,
            x = ~Period,
            y = ~Total,
            type = "scatter",
            mode = "lines+markers",
            name = cat,
            hovertemplate = paste0(
              "%{x|%b %d, %Y}<br>",
              cat,
              ": $%{y:,.2f}<extra></extra>"
            )
          )
      }

      plt %>%
        layout(
          xaxis = list(title = if (unit == "week") "Week" else "Month"),
          yaxis = list(
            title = "Spending",
            tickprefix = "$",
            separatethousands = TRUE
          ),
          legend = list(orientation = "h", x = 0, y = -0.2),
          title = "Spending over time"
        )
    }
  })

  category_summary <- reactive({
    df <- expenses()
    if (nrow(df) == 0) {
      return(tibble::tibble(
        Category = character(),
        Subcategory = character(),
        Total = numeric(),
        Transactions = integer()
      ))
    }

    if (!is.null(input$report_month) && input$report_month != "all") {
      month_start <- as.Date(input$report_month)
      month_end <- ceiling_date(month_start, "month") - days(1)
      df <- df %>% filter(Date >= month_start & Date <= month_end)

      if (nrow(df) == 0) {
        return(tibble::tibble(
          Category = character(),
          Subcategory = character(),
          Total = numeric(),
          Transactions = integer()
        ))
      }
    }

    df %>%
      mutate(Subcategory = format_subcategory(Subcategory)) %>%
      group_by(Category, Subcategory) %>%
      summarise(
        Total = sum(Amount, na.rm = TRUE),
        Transactions = dplyr::n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total))
  })

  report_data <- reactive({
    categories <- category_summary()
    if (nrow(categories) == 0 && nrow(budgets()) == 0) {
      return(tibble::tibble())
    }

    report_date <- if (is.null(input$report_month) || input$report_month == "all") {
      Sys.Date()
    } else {
      as.Date(input$report_month)
    }

    # Time-variant budget logic:
    # 1. Filter budgets that started on or before the report date
    # 2. For each Category/Subcategory, pick the most recent one (SCD Type 2 snapshot)
    active_budgets <- budgets() %>%
      filter(EffectiveDate <= report_date) %>%
      group_by(Category, Subcategory) %>%
      slice_max(order_by = EffectiveDate, n = 1, with_ties = FALSE) %>%
      ungroup()

    active_budgets %>%
      mutate(
        Subcategory = format_subcategory(Subcategory),
        # Always use monthly limit for reporting to align with monthly view
        Limit = get_monthly_limit(Limit, Frequency)
      ) %>%
      full_join(categories, by = c("Category", "Subcategory")) %>%
      mutate(
        Limit = replace_na(Limit, 0),
        Total = replace_na(Total, 0),
        Transactions = replace_na(Transactions, 0L),
        Remaining = Limit - Total,
        Status = case_when(
          Limit == 0 & Total == 0 ~ "No activity",
          Limit == 0 & Total > 0 ~ "Over (no budget)",
          Total < Limit ~ "Under budget",
          Total == Limit ~ "On budget",
          TRUE ~ "Over budget"
        )
      ) %>%
      arrange(desc(Total))
  })

  output$report_table <- renderDT({
    data <- report_data()
    validate(need(nrow(data) > 0, "Add expenses or budgets to see the report."))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency(
        "Total",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      ) %>%
      formatCurrency(
        "Limit",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      ) %>%
      formatCurrency(
        "Remaining",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      ) %>%
      formatStyle(
        "Status",
        color = styleEqual(
          c("Under budget", "On budget", "Over budget", "Over (no budget)"),
          c("green", "black", "red", "red")
        )
      )
  })

  output$report_summary <- renderUI({
    data <- report_data()
    # If no data, nothing to summarize
    if (nrow(data) == 0) {
      return(NULL)
    }

    total_limit <- sum(data$Limit, na.rm = TRUE)
    total_spent <- sum(data$Total, na.rm = TRUE)
    diff <- total_limit - total_spent

    # Determine status style
    # Positive diff = Under budget (Good)
    # Negative diff = Over budget (Bad)

    status_color <- if (diff >= 0) "green" else "red"
    status_text <- if (diff >= 0) "Under Budget" else "Over Budget"
    icon_name <- if (diff >= 0) "check-circle" else "exclamation-circle"

    abs_diff <- abs(diff)

    tags$div(
      style = paste0(
        "background-color: #f8f9fa; padding: 15px; border-radius: 5px; ",
        "border-left: 5px solid ", status_color, "; margin-bottom: 20px;"
      ),
      fluidRow(
        column(
          width = 3,
          h4("Total Budget", style = "margin-top:0; color: #666;"),
          h3(scales::dollar(total_limit), style = "margin-top:5px;")
        ),
        column(
          width = 3,
          h4("Total Spent", style = "margin-top:0; color: #666;"),
          h3(scales::dollar(total_spent), style = "margin-top:5px;")
        ),
        column(
          width = 6,
          h4(paste("Result:", status_text), style = paste0("margin-top:0; color: ", status_color, ";")),
          h3(
            icon(icon_name),
            paste0(scales::dollar(abs_diff), " ", tolower(status_text)),
            style = paste0("margin-top:5px; color: ", status_color, ";")
          )
        )
      )
    )
  })

  output$category_table <- renderDT({
    summary <- category_summary()
    validate(need(nrow(summary) > 0, "Add expenses to see the summary."))

    datatable(
      summary,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency(
        "Total",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      )
  })

  output$category_plot <- renderPlot({
    expense_summary <- category_summary() %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)")
      ) %>%
      group_by(Category) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")

    budget_summary <- budgets() %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)"),
        Limit = get_monthly_limit(Limit, Frequency)
      ) %>%
      group_by(Category) %>%
      summarise(Limit = sum(Limit, na.rm = TRUE), .groups = "drop")

    summary <- full_join(expense_summary, budget_summary, by = "Category") %>%
      mutate(
        Total = replace_na(Total, 0),
        Limit = replace_na(Limit, 0),
        Percent = if_else(
          Limit > 0,
          (Total / Limit) * 100,
          if_else(Total > 0, 100, 0)
        ),
        Fill = if_else(
          (Limit > 0 & Total > Limit) | (Limit == 0 & Total > 0),
          "#d73027",
          "#1b9e77"
        )
      ) %>%
      arrange(Percent)

    validate(need(
      nrow(summary) > 0,
      "Add expenses or budgets to see the plot."
    ))

    max_percent <- max(summary$Percent, na.rm = TRUE)
    if (!is.finite(max_percent)) {
      max_percent <- 0
    }
    upper_limit <- max(100, ceiling(max_percent / 10) * 10)

    ggplot(summary, aes(x = reorder(Category, Percent), y = Percent)) +
      geom_col(aes(fill = Fill), show.legend = FALSE) +
      geom_hline(
        yintercept = 100,
        linetype = "dashed",
        color = "#333333",
        linewidth = 0.8
      ) +
      labs(
        x = "Category",
        y = "Percent of budget",
        title = "Spending by category"
      ) +
      scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        limits = c(0, upper_limit)
      ) +
      scale_fill_identity() +
      theme_minimal(base_size = 14)
  })

  observeEvent(input$email_report, {
    req(input$report_email_to)

    if (!grepl("@", input$report_email_to)) {
      showNotification("Please enter a valid email address.", type = "error")
      return()
    }

    showNotification("Generating report...", id = "report_msg", duration = NULL, type = "message")

    tryCatch(
      {
        # Grab the reactive data exactly as currently computed in the app
        rd <- report_data()
        cs <- category_summary()

        # Calculate recent expenses for 4-week chart
        max_date <- if (is.null(input$report_month) || input$report_month == "all") {
          Sys.Date()
        } else {
          ceiling_date(as.Date(input$report_month), "month") - days(1)
        }
        re <- expenses() %>%
          filter(Date >= (max_date - weeks(4)) & Date <= max_date)

        # The active_budgets structure is equivalent to report_data except it isolates the budget.
        # Since we just need limit by category, we extract that calculation from `budgets()`
        report_date <- if (is.null(input$report_month) || input$report_month == "all") Sys.Date() else as.Date(input$report_month)
        ab <- budgets() %>%
          filter(EffectiveDate <= report_date & (is.na(ConclusionDate) | ConclusionDate >= Sys.Date())) %>%
          group_by(Category, Subcategory) %>%
          slice_max(order_by = EffectiveDate, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          mutate(
            Subcategory = format_subcategory(Subcategory),
            Limit = get_monthly_limit(Limit, Frequency)
          )


        rd_path <- tempfile(fileext = ".rds")
        cs_path <- tempfile(fileext = ".rds")
        ab_path <- tempfile(fileext = ".rds")
        re_path <- tempfile(fileext = ".rds")

        saveRDS(rd, rd_path)
        saveRDS(cs, cs_path)
        saveRDS(ab, ab_path)
        saveRDS(re, re_path)

        # We output using blastula's specific render function that knows how to package R Markdown internally
        email <- blastula::render_email(
          "monthly_report.Rmd",
          envir = new.env(),
          render_options = list(
            params = list(
              report_month = input$report_month,
              report_data_path = rd_path,
              category_summary_path = cs_path,
              active_budgets_path = ab_path,
              recent_expenses_path = re_path
            )
          )
        )
        showNotification("Sending email...", id = "report_msg_2", duration = NULL, type = "message")

        month_str <- if (input$report_month == "all") "all time" else format(as.Date(input$report_month), "%B %Y")

        blastula::smtp_send(
          email = email,
          from = "carsonslater7@gmail.com",
          to = input$report_email_to,
          subject = paste("Budget Report -", month_str),
          credentials = blastula::creds_envvar(
            user = "carsonslater7@gmail.com",
            pass_envvar = "SMTP_PASSWORD",
            host = "smtp.gmail.com",
            port = 465,
            use_ssl = TRUE
          )
        )

        removeNotification("report_msg")
        removeNotification("report_msg_2")
        showNotification("Report generated and emailed successfully!", type = "message")

        # Cleanup temporary files
        if (file.exists("monthly_report.knit.md")) unlink("monthly_report.knit.md")
        if (file.exists(rd_path)) unlink(rd_path)
        if (file.exists(cs_path)) unlink(cs_path)
        if (file.exists(ab_path)) unlink(ab_path)
      },
      error = function(e) {
        removeNotification("report_msg")
        if (exists("report_msg_2")) removeNotification("report_msg_2")
        showNotification(paste("Error generating/sending report:", e$message), type = "error", duration = 15)
      }
    )
  })
}

shinyApp(ui = ui, server = server)
