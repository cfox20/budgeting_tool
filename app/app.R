library(shiny)
library(DT)
library(dplyr)
library(tidyr)

library(ggplot2)

# Data configuration -----------------------------------------------------------

data_dir <- normalizePath(file.path("..", "data"), winslash = "/", mustWork = FALSE)
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}

expenses_path <- file.path(data_dir, "expenses.csv")
budgets_path <- file.path(data_dir, "category_budget.csv")
income_path <- file.path(data_dir, "income_sources.csv")


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
  Limit = numeric()
)

default_payers <- c("Joint", "Partner 1", "Partner 2")

clean_subcategory <- function(x) {
  x <- tidyr::replace_na(x, "")
  trimws(x)
}

load_expenses <- function() {
  if (!file.exists(expenses_path)) {
    return(empty_expenses)
  }

  df <- readr::read_csv(
    expenses_path,
    col_types = cols(
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
    arrange(Date)
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
      Limit = col_double()
    ),
    show_col_types = FALSE
  )

  if (!"Limit" %in% names(df) && "Target" %in% names(df)) {
    df <- dplyr::rename(df, Limit = Target)
  }

  df %>%
    mutate(
      Category = tidyr::replace_na(Category, ""),
      Subcategory = clean_subcategory(Subcategory),
      Limit = replace_na(Limit, 0)
    ) %>%
    arrange(Category, Subcategory)
}

load_income_sources <- function() {
  if (!file.exists(income_path)) {
    return(tibble::tibble(Source = character(), Amount = numeric()))
  }

  readr::read_csv(
    income_path,
    col_types = cols(
      Source = col_character(),
      Amount = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    mutate(
      Source = tidyr::replace_na(Source, ""),
      Amount = replace_na(Amount, 0)
    )
}

write_expenses <- function(df) {
  readr::write_csv(df, expenses_path, na = "")
}

write_budgets <- function(df) {
  readr::write_csv(df, budgets_path, na = "")
}

format_subcategory <- function(value) {
  value <- clean_subcategory(value)
  ifelse(nzchar(value), value, "(Unspecified)")
}

# User interface --------------------------------------------------------------
ui <- navbarPage(
  title = "Household Budgeting",
  tabPanel(
    "Expenses",
    fluidPage(
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
            options = list(placeholder = "Select or add a category", create = TRUE)
          ),
          selectizeInput(
            "expense_subcategory",
            "Subcategory",
            choices = NULL,
            options = list(placeholder = "Select or add a subcategory", create = TRUE)
          ),
          numericInput("expense_amount", "Amount", value = NA, min = 0, step = 0.01),
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
          numericInput("income", "Monthly income", value = NA, min = 0, step = 50),
          actionButton("set_income", "Save income", class = "btn-secondary"),
          br(),
          br(),
          h4("Add or update a budget line"),
          selectizeInput(
            "budget_category",
            "Category",
            choices = NULL,
            options = list(placeholder = "Select or add a category", create = TRUE)
          ),
          selectizeInput(
            "budget_subcategory",
            "Subcategory",
            choices = NULL,
            options = list(placeholder = "Select or add a subcategory", create = TRUE)
          ),
          numericInput("budget_limit", "Monthly limit", value = NA, min = 0, step = 10),
          actionButton("add_budget", "Save budget", class = "btn-primary")
        ),
        column(
          width = 8,
          h3("Budgets"),
          uiOutput("income_summary"),
          DTOutput("budget_table")
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
          DTOutput("report_table"),
          br(),
          h3("Spending by category"),
          plotOutput("category_plot", height = "350px"),
          br(),
          h3("Category and subcategory details"),
          DTOutput("category_table")
        )
      )
    )
  )
)

# Server logic ----------------------------------------------------------------

server <- function(input, output, session) {
  expenses <- reactiveVal(load_expenses())
  budgets <- reactiveVal(load_budgets())
  income_sources <- reactiveVal(load_income_sources())

  initial_income <- sum(income_sources()$Amount, na.rm = TRUE)
  monthly_income <- reactiveVal(ifelse(is.finite(initial_income), initial_income, NA_real_))

  observeEvent(TRUE, {
    updateNumericInput(session, "income", value = monthly_income())
  }, once = TRUE)

  observe({
    categories <- budgets() %>%
      filter(nzchar(Category)) %>%
      distinct(Category) %>%
      arrange(Category) %>%
      pull(Category)

    updateSelectizeInput(
      session,
      "expense_category",
      choices = categories,
      selected = if (length(categories) && input$expense_category %in% categories) input$expense_category else NULL,
      server = FALSE
    )

    updateSelectizeInput(
      session,
      "budget_category",
      choices = categories,
      selected = if (length(categories) && input$budget_category %in% categories) input$budget_category else NULL,
      server = FALSE
    )
  })

  observe({
    payers <- expenses() %>%
      filter(nzchar(Payer)) %>%
      distinct(Payer) %>%
      arrange(Payer) %>%
      pull(Payer)

    payers <- unique(c(default_payers, payers))

    updateSelectizeInput(
      session,
      "expense_payer",
      choices = payers,
      selected = if (length(payers) && input$expense_payer %in% payers) input$expense_payer else NULL,
      server = FALSE
    )
  })

  observeEvent(input$expense_category, {
    req(!is.null(input$expense_category))
    subchoices <- budgets() %>%
      filter(Category == input$expense_category, nzchar(Subcategory)) %>%
      distinct(Subcategory) %>%
      arrange(Subcategory) %>%
      pull(Subcategory)

    updateSelectizeInput(
      session,
      "expense_subcategory",
      choices = subchoices,
      selected = if (length(subchoices) && input$expense_subcategory %in% subchoices) input$expense_subcategory else NULL,
      server = FALSE
    )
  })

  observeEvent(input$budget_category, {
    if (is.null(input$budget_category) || !nzchar(input$budget_category)) {
      updateSelectizeInput(session, "budget_subcategory", choices = character(0), selected = NULL, server = FALSE)
      return()
    }

    subchoices <- budgets() %>%
      filter(Category == input$budget_category, nzchar(Subcategory)) %>%
      distinct(Subcategory) %>%
      arrange(Subcategory) %>%
      pull(Subcategory)

    updateSelectizeInput(
      session,
      "budget_subcategory",
      choices = subchoices,
      selected = if (length(subchoices) && input$budget_subcategory %in% subchoices) input$budget_subcategory else NULL,
      server = FALSE
    )
  })

  observeEvent(input$add_expense, {
    description <- trimws(input$expense_description)
    category <- trimws(input$expense_category)
    subcategory <- trimws(input$expense_subcategory)
    payer <- trimws(input$expense_payer)

    validate(
      need(!is.null(input$expense_date) && !is.na(input$expense_date), "Please supply a date."),
      need(nzchar(description), "Describe the expense."),
      need(nzchar(category), "Choose a category."),
      need(!is.null(input$expense_amount) && !is.na(input$expense_amount) && input$expense_amount > 0, "Enter a positive amount.")
    )

    entry <- tibble::tibble(
      Date = as.Date(input$expense_date),
      Description = description,
      Category = category,
      Subcategory = clean_subcategory(subcategory),
      Amount = as.numeric(input$expense_amount),
      Payer = payer
    )

    updated <- bind_rows(expenses(), entry) %>% arrange(Date)
    expenses(updated)
    write_expenses(updated)

    updateTextInput(session, "expense_description", value = "")
    updateNumericInput(session, "expense_amount", value = NA)
    updateSelectizeInput(session, "expense_subcategory", selected = NULL, server = FALSE)
    showNotification("Expense added.", type = "message")
  })

  observeEvent(input$add_budget, {
    category <- trimws(input$budget_category)
    subcategory <- clean_subcategory(input$budget_subcategory)

    validate(
      need(nzchar(category), "Provide a category."),
      need(!is.null(input$budget_limit) && !is.na(input$budget_limit) && input$budget_limit >= 0, "Enter a non-negative limit.")
    )

    new_budget <- tibble::tibble(
      Category = category,
      Subcategory = subcategory,
      Limit = as.numeric(input$budget_limit)
    )

    current <- budgets()
    match_idx <- which(
      tolower(current$Category) == tolower(category) &
        tolower(clean_subcategory(current$Subcategory)) == tolower(subcategory)
    )

    if (length(match_idx) > 0) {
      current$Limit[match_idx[1]] <- new_budget$Limit
      updated <- current
    } else {
      updated <- bind_rows(current, new_budget) %>% arrange(Category, Subcategory)
    }

    budgets(updated)
    write_budgets(updated)

    updateSelectizeInput(session, "budget_subcategory", selected = NULL, server = FALSE)
    updateNumericInput(session, "budget_limit", value = NA)
    showNotification("Budget saved.", type = "message")
  })

  observeEvent(input$set_income, {
    validate(
      need(!is.null(input$income) && !is.na(input$income) && input$income >= 0, "Enter a non-negative income.")
    )
    monthly_income(as.numeric(input$income))
    showNotification("Income updated.", type = "message")
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
      "Logged expenses: ", totals$Count,
      " | Total spent: ", dollar(totals$Total)
    )
  })

  output$expense_table <- renderDT({
    data <- expenses() %>%
      mutate(Subcategory = format_subcategory(Subcategory))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency("Amount", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  output$budget_table <- renderDT({
    data <- budgets() %>%
      mutate(Subcategory = format_subcategory(Subcategory))

    validate(need(nrow(data) > 0, "Add budgets to track your plan."))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency("Limit", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  output$income_summary <- renderUI({
    income <- monthly_income()
    budget_total <- sum(budgets()$Limit, na.rm = TRUE)
    remaining <- if (is.na(income)) NA_real_ else income - budget_total

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
        strong("Unallocated:"),
        if (is.na(remaining)) "--" else dollar(remaining)
      )
    )
  })

  category_summary <- reactive({
    df <- expenses()
    if (nrow(df) == 0) {
      return(tibble::tibble(Category = character(), Subcategory = character(), Total = numeric(), Transactions = integer()))
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

    budgets() %>%
      mutate(Subcategory = format_subcategory(Subcategory)) %>%
      full_join(categories, by = c("Category", "Subcategory")) %>%
      mutate(
        Limit = replace_na(Limit, 0),
        Total = replace_na(Total, 0),
        Transactions = replace_na(Transactions, 0L),
        Remaining = Limit - Total,
        Status = case_when(
          Limit == 0 & Total == 0 ~ "No activity",
          Limit == 0 & Total > 0 ~ "Over (no budget)",
          Total <= Limit ~ "Within budget",
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
      formatCurrency("Total", currency = "$", interval = 3, mark = ",", digits = 2) %>%
      formatCurrency("Limit", currency = "$", interval = 3, mark = ",", digits = 2) %>%
      formatCurrency("Remaining", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  output$category_table <- renderDT({
    summary <- category_summary()
    validate(need(nrow(summary) > 0, "Add expenses to see the summary."))

    datatable(
      summary,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency("Total", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  output$category_plot <- renderPlot({
    summary <- category_summary() %>%
      group_by(Category) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
      arrange(Total)
    validate(need(nrow(summary) > 0, "Add expenses to see the plot."))

    ggplot(summary, aes(x = reorder(Category, Total), y = Total)) +
      geom_col(fill = "#1b9e77") +
      coord_flip() +
      labs(x = "Category", y = "Total spent", title = "Spending by category") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
