library(shiny)
library(shinythemes)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

# -----------------------------------------------------------------------------
# File helpers ---------------------------------------------------------------
resolve_data_dir <- function() {
  env_dir <- Sys.getenv("R_SHINY_BUDGET_DATA_DIR", unset = NA_character_)
  if (!is.na(env_dir) && nzchar(env_dir)) {
    return(normalizePath(env_dir, mustWork = FALSE))
  }
  default_dir <- file.path(normalizePath("..", winslash = "/", mustWork = FALSE), "user_data")
  normalizePath(default_dir, winslash = "/", mustWork = FALSE)
}

data_dir <- resolve_data_dir()
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}
message("Using budgeting data directory: ", data_dir)

paths <- list(
  expenses = file.path(data_dir, "expenses.csv"),
  expenses_backup = file.path(data_dir, "expenses_backup.csv"),
  income = file.path(data_dir, "income_sources.csv"),
  budget = file.path(data_dir, "category_budget.csv")
)

empty_expenses <- tibble(
  date = character(),
  description = character(),
  category = character(),
  amount = double(),
  payer = character(),
  account = character()
)

empty_income <- tibble(source = character(), amount = double())
empty_budget <- tibble(category = character(), target_amount = double())

safe_read <- function(path, col_types, empty_frame, transform) {
  if (!file.exists(path)) {
    return(empty_frame)
  }
  tryCatch(
    {
      df <- readr::read_csv(
        path,
        col_types = col_types,
        show_col_types = FALSE,
        progress = FALSE
      )
      transform(df)
    },
    error = function(err) {
      warning(sprintf("Failed to read %s: %s", basename(path), conditionMessage(err)))
      empty_frame
    }
  )
}

prepare_expenses <- function(df) {
  df <- df %>%
    mutate(
      date = as_date(ymd(date, quiet = TRUE)),
      date = if_else(is.na(date), today(), date),
      date = format(date, "%Y-%m-%d"),
      description = replace_na(trimws(as.character(description)), ""),
      category = replace_na(trimws(as.character(category)), ""),
      payer = replace_na(trimws(as.character(payer)), ""),
      account = replace_na(trimws(as.character(account)), ""),
      amount = replace_na(as.numeric(amount), 0)
    )
  for (col in names(empty_expenses)) {
    if (!col %in% names(df)) {
      df[[col]] <- empty_expenses[[col]]
    }
  }
  df %>% select(all_of(names(empty_expenses)))
}

prepare_numeric_frame <- function(df, name_col, amount_col) {
  df <- df %>%
    mutate(
      !!name_col := replace_na(trimws(as.character(.data[[name_col]])), ""),
      !!amount_col := replace_na(as.numeric(.data[[amount_col]]), 0)
    )
  for (col in c(name_col, amount_col)) {
    if (!col %in% names(df)) {
      df[[col]] <- if (col == amount_col) 0 else ""
    }
  }
  df %>% select(all_of(c(name_col, amount_col)))
}

read_expenses <- function() {
  safe_read(
    paths$expenses,
    col_types = cols(
      date = col_character(),
      description = col_character(),
      category = col_character(),
      amount = col_double(),
      payer = col_character(),
      account = col_character(),
      .default = col_guess()
    ),
    empty_expenses,
    function(df) {
      df %>%
        bind_rows(empty_expenses) %>%
        select(all_of(names(empty_expenses))) %>%
        prepare_expenses()
    }
  )
}

read_income <- function() {
  safe_read(
    paths$income,
    col_types = cols(
      source = col_character(),
      amount = col_double(),
      .default = col_guess()
    ),
    empty_income,
    function(df) {
      df %>%
        bind_rows(empty_income) %>%
        select(all_of(names(empty_income))) %>%
        prepare_numeric_frame("source", "amount")
    }
  )
}

read_budget <- function() {
  safe_read(
    paths$budget,
    col_types = cols(
      category = col_character(),
      target_amount = col_double(),
      .default = col_guess()
    ),
    empty_budget,
    function(df) {
      df %>%
        bind_rows(empty_budget) %>%
        select(all_of(names(empty_budget))) %>%
        prepare_numeric_frame("category", "target_amount")
    }
  )
}

write_expenses <- function(df) {
  df <- prepare_expenses(df)
  if (file.exists(paths$expenses)) {
    file.copy(paths$expenses, paths$expenses_backup, overwrite = TRUE)
  }
  readr::write_csv(df, paths$expenses, progress = FALSE)
}

write_income <- function(df) {
  df <- prepare_numeric_frame(df, "source", "amount")
  readr::write_csv(df, paths$income, progress = FALSE)
}

write_budget <- function(df) {
  df <- prepare_numeric_frame(df, "category", "target_amount")
  readr::write_csv(df, paths$budget, progress = FALSE)
}

clean_choices <- function(values) {
  values <- values[!is.na(values) & nzchar(values)]
  sort(unique(values))
}

category_summary <- function(expenses, budgets) {
  if (!nrow(expenses)) {
    return(tibble(category = character(), spent = double(), target_amount = double(), difference = double()))
  }
  summary <- expenses %>%
    mutate(date = ymd(date, quiet = TRUE)) %>%
    filter(!is.na(date)) %>%
    group_by(category) %>%
    summarise(spent = sum(amount, na.rm = TRUE), .groups = "drop") %>%
    full_join(budgets, by = "category") %>%
    mutate(
      target_amount = replace_na(target_amount, 0),
      spent = replace_na(spent, 0),
      difference = spent - target_amount
    ) %>%
    arrange(desc(spent))
  summary
}

# -----------------------------------------------------------------------------
# UI -------------------------------------------------------------------------
ui <- navbarPage(
  title = "Household Budgeting",
  theme = shinytheme("flatly"),
  tabPanel(
    "Expenses",
    fluidRow(
      column(
        width = 4,
        h3("Add a new expense"),
        dateInput("expense_date", "Date", value = Sys.Date()),
        textInput("expense_description", "Description"),
        selectizeInput(
          "expense_category",
          "Category",
          choices = NULL,
          options = list(create = TRUE, persist = TRUE)
        ),
        numericInput("expense_amount", "Amount", value = 0, min = 0, step = 0.01),
        selectizeInput(
          "expense_payer",
          "Payer",
          choices = NULL,
          options = list(create = TRUE, persist = TRUE)
        ),
        selectizeInput(
          "expense_account",
          "Account",
          choices = NULL,
          options = list(create = TRUE, persist = TRUE)
        ),
        actionButton("add_expense", "Add expense", class = "btn-primary")
      ),
      column(
        width = 8,
        h3("Logged expenses"),
        p("Double-click a cell to edit it directly. Select a row to delete it."),
        DTOutput("expenses_table"),
        br(),
        actionButton("delete_expense", "Delete selected"),
        actionButton("reload_expenses", "Reload from disk"),
        actionButton("save_expenses", "Save changes", class = "btn-success")
      )
    )
  ),
  tabPanel(
    "Budget planning",
    fluidRow(
      column(
        width = 6,
        h3("Income sources"),
        p("Add each recurring monthly income stream."),
        actionButton("add_income", "Add income row"),
        actionButton("delete_income", "Delete selected"),
        actionButton("save_income", "Save income", class = "btn-success"),
        DTOutput("income_table")
      ),
      column(
        width = 6,
        h3("Category targets"),
        p("Set monthly targets for each spending category."),
        actionButton("add_budget", "Add budget row"),
        actionButton("delete_budget", "Delete selected"),
        actionButton("save_budget", "Save targets", class = "btn-success"),
        DTOutput("budget_table")
      )
    )
  ),
  tabPanel(
    "Reports",
    fluidRow(
      column(
        width = 4,
        dateRangeInput(
          "report_dates",
          "Date range",
          start = Sys.Date() - 29,
          end = Sys.Date(),
          max = Sys.Date()
        ),
        verbatimTextOutput("report_summary")
      ),
      column(
        width = 8,
        plotOutput("spending_plot", height = 320)
      )
    ),
    fluidRow(
      column(
        width = 12,
        h4("Category overview"),
        DTOutput("category_table")
      )
    ),
    fluidRow(
      column(
        width = 6,
        h4("Over budget"),
        DTOutput("over_budget_table")
      ),
      column(
        width = 6,
        h4("Under budget"),
        DTOutput("under_budget_table")
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server ---------------------------------------------------------------------
server <- function(input, output, session) {
  expenses <- reactiveVal(read_expenses())
  income <- reactiveVal(read_income())
  budgets <- reactiveVal(read_budget())

  observe({
    cats <- clean_choices(c(expenses()[["category"]], budgets()[["category"]]))
    updateSelectizeInput(session, "expense_category", choices = cats, server = TRUE)

    payers <- clean_choices(expenses()[["payer"]])
    updateSelectizeInput(session, "expense_payer", choices = payers, server = TRUE)

    accounts <- clean_choices(expenses()[["account"]])
    updateSelectizeInput(session, "expense_account", choices = accounts, server = TRUE)
  })

  observeEvent(input$add_expense, {
    amount_value <- input$expense_amount
    if (is.null(amount_value) || is.na(amount_value)) {
      amount_value <- 0
    }
    new_row <- tibble(
      date = as.character(input$expense_date),
      description = trimws(input$expense_description),
      category = trimws(input$expense_category),
      amount = round(as.numeric(amount_value), 2),
      payer = trimws(input$expense_payer),
      account = trimws(input$expense_account)
    )
    updated <- bind_rows(expenses(), new_row) %>% prepare_expenses()
    expenses(updated)

    updateTextInput(session, "expense_description", value = "")
    updateNumericInput(session, "expense_amount", value = 0)
  })

  observeEvent(input$delete_expense, {
    selected <- input$expenses_table_rows_selected
    if (length(selected)) {
      df <- expenses()
      df <- df[-selected, , drop = FALSE]
      expenses(df)
    }
  })

  observeEvent(input$reload_expenses, {
    expenses(read_expenses())
  })

  observeEvent(input$expenses_table_cell_edit, {
    info <- input$expenses_table_cell_edit
    df <- expenses()
    row <- info$row
    col <- info$col + 1
    col_name <- names(df)[col]
    value <- info$value

    if (col_name == "amount") {
      value <- as.numeric(value)
      if (is.na(value)) value <- 0
    } else {
      value <- trimws(as.character(value))
    }
    df[row, col_name] <- value
    expenses(prepare_expenses(df))
  })

  observeEvent(input$save_expenses, {
    df <- expenses()
    if (!nrow(df)) {
      showNotification("There are no expenses to save yet.", type = "warning")
      return()
    }
    preview <- tail(df, 15)
    output$save_preview <- renderDT({
      DT::datatable(
        preview,
        options = list(pageLength = min(nrow(preview), 5), dom = "tip"),
        rownames = FALSE
      ) %>%
        formatCurrency("amount", currency = "$", interval = 3, mark = ",")
    })
    showModal(
      modalDialog(
        title = "Confirm expense save",
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_save_expenses", "Save", class = "btn-primary")
        ),
        p("Review the most recent entries. Saving will also update the backup copy."),
        DTOutput("save_preview")
      )
    )
  })

  observeEvent(input$confirm_save_expenses, {
    write_expenses(expenses())
    expenses(read_expenses())
    removeModal()
    showNotification("Expenses saved", type = "message")
  })

  output$expenses_table <- renderDT({
    DT::datatable(
      expenses(),
      selection = "single",
      editable = "cell",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatCurrency("amount", currency = "$", interval = 3, mark = ",")
  })

  # Income management --------------------------------------------------------
  observeEvent(input$add_income, {
    income(bind_rows(income(), tibble(source = "", amount = 0)))
  })

  observeEvent(input$delete_income, {
    rows <- input$income_table_rows_selected
    if (length(rows)) {
      df <- income()
      df <- df[-rows, , drop = FALSE]
      income(df)
    }
  })

  observeEvent(input$income_table_cell_edit, {
    info <- input$income_table_cell_edit
    df <- income()
    col <- names(df)[info$col + 1]
    value <- if (col == "amount") as.numeric(info$value) else trimws(as.character(info$value))
    if (col == "amount" && is.na(value)) value <- 0
    df[info$row, col] <- value
    income(prepare_numeric_frame(df, "source", "amount"))
  })

  observeEvent(input$save_income, {
    cleaned <- prepare_numeric_frame(income(), "source", "amount")
    write_income(cleaned)
    income(cleaned)
    showNotification("Income saved", type = "message")
  })

  output$income_table <- renderDT({
    DT::datatable(
      income(),
      selection = "single",
      editable = "cell",
      rownames = FALSE,
      options = list(dom = "t", pageLength = 8)
    ) %>%
      formatCurrency("amount", currency = "$", interval = 3, mark = ",")
  })

  # Budget management --------------------------------------------------------
  observeEvent(input$add_budget, {
    budgets(bind_rows(budgets(), tibble(category = "", target_amount = 0)))
  })

  observeEvent(input$delete_budget, {
    rows <- input$budget_table_rows_selected
    if (length(rows)) {
      df <- budgets()
      df <- df[-rows, , drop = FALSE]
      budgets(df)
    }
  })

  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df <- budgets()
    col <- names(df)[info$col + 1]
    value <- if (col == "target_amount") as.numeric(info$value) else trimws(as.character(info$value))
    if (col == "target_amount" && is.na(value)) value <- 0
    df[info$row, col] <- value
    budgets(prepare_numeric_frame(df, "category", "target_amount"))
  })

  observeEvent(input$save_budget, {
    cleaned <- prepare_numeric_frame(budgets(), "category", "target_amount")
    write_budget(cleaned)
    budgets(cleaned)
    showNotification("Budget targets saved", type = "message")
  })

  output$budget_table <- renderDT({
    DT::datatable(
      budgets(),
      selection = "single",
      editable = "cell",
      rownames = FALSE,
      options = list(dom = "t", pageLength = 10)
    ) %>%
      formatCurrency("target_amount", currency = "$", interval = 3, mark = ",")
  })

  # Reports ------------------------------------------------------------------
  filtered_expenses <- reactive({
    df <- expenses()
    if (!nrow(df)) return(df)
    range <- input$report_dates
    df <- df %>% mutate(date = ymd(date, quiet = TRUE)) %>% filter(!is.na(date))
    if (!is.null(range) && length(range) == 2) {
      df <- df %>% filter(date >= range[1], date <= range[2])
    }
    df
  })

  category_data <- reactive({
    category_summary(filtered_expenses(), budgets())
  })

  output$report_summary <- renderText({
    df <- filtered_expenses()
    if (!nrow(df)) {
      return("No expenses recorded for the selected range yet.")
    }
    total <- sum(df$amount, na.rm = TRUE)
    avg <- mean(df$amount, na.rm = TRUE)
    sprintf("Total spent: %s\nAverage transaction: %s", dollar(total), dollar(avg))
  })

  output$spending_plot <- renderPlot({
    df <- filtered_expenses()
    if (!nrow(df)) return(NULL)
    summary <- df %>%
      group_by(category) %>%
      summarise(spent = sum(amount, na.rm = TRUE), .groups = "drop")
    if (!nrow(summary)) return(NULL)

    ggplot(summary, aes(x = reorder(category, spent), y = spent)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Amount", title = "Spending by category") +
      theme_minimal(base_size = 14)
  })

  output$category_table <- renderDT({
    df <- category_data()
    if (!nrow(df)) {
      return(DT::datatable(tibble(message = "No expenses yet."), options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(1, "desc")))
    ) %>%
      formatCurrency(c("spent", "target_amount", "difference"), currency = "$", interval = 3, mark = ",")
  })

  output$over_budget_table <- renderDT({
    df <- category_data() %>% filter(target_amount > 0, spent > target_amount)
    if (!nrow(df)) {
      return(DT::datatable(tibble(message = "None"), options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(df, rownames = FALSE, options = list(dom = "t")) %>%
      formatCurrency(c("spent", "target_amount", "difference"), currency = "$", interval = 3, mark = ",")
  })

  output$under_budget_table <- renderDT({
    df <- category_data() %>% filter(target_amount > 0, spent <= target_amount)
    if (!nrow(df)) {
      return(DT::datatable(tibble(message = "None"), options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(df, rownames = FALSE, options = list(dom = "t")) %>%
      formatCurrency(c("spent", "target_amount", "difference"), currency = "$", interval = 3, mark = ",")
  })
}

# run ------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
