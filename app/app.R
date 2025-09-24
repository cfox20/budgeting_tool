library(shiny)
library(DT)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Directory configuration ---------------------------------------------------

data_dir <- file.path("..", "data")
expenses_path <- file.path(data_dir, "expenses.csv")
backup_path <- file.path(data_dir, "expenses_backup.csv")

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}

# Helper utilities ----------------------------------------------------------

empty_expenses <- data.frame(
  Date = as.Date(character()),
  Description = character(),
  Category = character(),
  Amount = numeric(),
  Payer = character(),
  stringsAsFactors = FALSE
)

default_categories <- c(
  "Housing", "Utilities", "Groceries", "Dining Out", "Transportation",
  "Healthcare", "Insurance", "Entertainment", "Personal Care",
  "Debt Payments", "Savings", "Gifts", "Travel", "Other"
)

default_payers <- c("Joint", "Partner 1", "Partner 2")

load_expenses <- function() {
  if (file.exists(expenses_path)) {
    readr::read_csv(
      expenses_path,
      col_types = cols(
        Date = col_date(),
        Description = col_character(),
        Category = col_character(),
        Amount = col_double(),
        Payer = col_character()
      ),
      show_col_types = FALSE
    )
  } else {
    empty_expenses
  }
}

backup_preview <- function() {
  if (file.exists(backup_path)) {
    readr::read_csv(
      backup_path,
      col_types = cols(
        Date = col_date(),
        Description = col_character(),
        Category = col_character(),
        Amount = col_double(),
        Payer = col_character()
      ),
      show_col_types = FALSE
    )
  } else {
    NULL
  }
}

coerce_value <- function(value, column_name) {
  if (column_name == "Date") {
    parsed <- suppressWarnings(as.Date(value))
    if (is.na(parsed)) stop("Please supply a valid date (YYYY-MM-DD).")
    return(parsed)
  }
  if (column_name == "Amount") {
    parsed <- suppressWarnings(as.numeric(value))
    if (is.na(parsed)) stop("Please supply a numeric amount.")
    return(parsed)
  }
  value
}

# UI ------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Household Expense Tracker"),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      title = "Enter Expenses",
      fluidRow(
        column(
          width = 4,
          h3("Add a new expense"),
          dateInput("date", "Date", value = Sys.Date()),
          textInput("description", "Description"),
          numericInput("amount", "Amount", value = NA, min = 0, step = 0.01),
          selectizeInput(
            "category",
            "Category",
            choices = default_categories,
            options = list(create = TRUE, placeholder = "Select or type a category")
          ),
          selectizeInput(
            "payer",
            "Payer",
            choices = default_payers,
            options = list(create = TRUE, placeholder = "Select or type who paid")
          ),
          actionButton("add_expense", "Add expense", class = "btn-primary"),
          br(),
          br(),
          actionButton("save_expenses", "Save changes to file", class = "btn-success")
        ),
        column(
          width = 8,
          h3("Current expenses"),
          p("You can edit cells directly in this table before saving."),
          DTOutput("expense_table"),
          br(),
          verbatimTextOutput("save_status")
        )
      )
    ),
    tabPanel(
      title = "Reports",
      fluidRow(
        column(
          width = 4,
          h3("Filters"),
          dateRangeInput(
            "report_range",
            "Reporting range",
            start = Sys.Date() - months(1),
            end = Sys.Date()
          ),
          checkboxInput("exclude_zero", "Exclude zero or negative amounts", value = TRUE)
        ),
        column(
          width = 8,
          h3("Spending by category"),
          DTOutput("category_summary"),
          br(),
          plotOutput("category_plot", height = "400px")
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("Expense detail"),
          DTOutput("detailed_table")
        )
      )
    )
  )
)

# Server --------------------------------------------------------------------

server <- function(input, output, session) {
  expenses_data <- reactiveVal(load_expenses())

  observe({
    df <- expenses_data()
    updateSelectizeInput(
      session,
      "category",
      choices = sort(unique(c(default_categories, df$Category))),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "payer",
      choices = sort(unique(c(default_payers, df$Payer))),
      server = TRUE
    )
  })

  observeEvent(input$add_expense, {
    description <- trimws(input$description)
    validate(
      need(!is.null(input$date) && !is.na(input$date), "Please provide a date."),
      need(nzchar(description), "Please provide a description."),
      need(!is.null(input$amount) && !is.na(input$amount) && input$amount != "", "Please provide an amount."),
      need(!is.null(input$category) && nzchar(input$category), "Please choose a category."),
      need(!is.null(input$payer) && nzchar(input$payer), "Please specify who paid.")
    )

    new_entry <- data.frame(
      Date = as.Date(input$date),
      Description = description,
      Category = input$category,
      Amount = as.numeric(input$amount),
      Payer = input$payer,
      stringsAsFactors = FALSE
    )

    updated <- dplyr::bind_rows(expenses_data(), new_entry)
    expenses_data(updated)

    updateTextInput(session, "description", value = "")
    updateNumericInput(session, "amount", value = NA)
  })

  output$expense_table <- renderDT({
    datatable(
      expenses_data(),
      editable = "cell",
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 25, 50))
    ) %>%
      formatCurrency("Amount", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  observeEvent(input$expense_table_cell_edit, {
    info <- input$expense_table_cell_edit
    df <- expenses_data()
    row <- info$row
    col <- info$col + 1
    column_name <- colnames(df)[col]

    tryCatch({
      df[row, column_name] <- coerce_value(info$value, column_name)
      expenses_data(df)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
    })
  })

  save_preview_backup <- eventReactive(input$save_expenses, {
    bk <- backup_preview()
    if (is.null(bk)) {
      data.frame(Message = "No existing backup file found.")
    } else {
      head(bk, 10)
    }
  })

  save_preview_new <- eventReactive(input$save_expenses, {
    preview <- expenses_data()
    if (nrow(preview) == 0) {
      data.frame(Message = "No expenses to save yet.")
    } else {
      head(preview, 10)
    }
  })

  output$backup_preview <- renderTable({
    save_preview_backup()
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

  output$new_data_preview <- renderTable({
    save_preview_new()
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

  observeEvent(input$save_expenses, {
    showModal(
      modalDialog(
        title = "Confirm save",
        size = "l",
        strong("Please review the backup and the new data before confirming."),
        br(),
        h4("Current backup (first 10 rows)"),
        tableOutput("backup_preview"),
        br(),
        h4("New data to be written (first 10 rows)"),
        tableOutput("new_data_preview"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_save", "Overwrite backup and save", class = "btn-danger")
        ),
        easyClose = FALSE
      )
    )
  })

  observeEvent(input$confirm_save, {
    removeModal()

    df <- expenses_data()
    if (nrow(df) == 0) {
      showNotification("There are no expenses to save.", type = "warning")
      return(NULL)
    }

    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

    if (file.exists(expenses_path)) {
      copied <- file.copy(expenses_path, backup_path, overwrite = TRUE)
      if (!copied) {
        showNotification("Failed to update the backup file.", type = "error")
        return(NULL)
      }
    }

    readr::write_csv(df, expenses_path)
    output$save_status <- renderText({
      paste0("Saved ", nrow(df), " expenses to ", normalizePath(expenses_path))
    })
    showNotification("Expenses saved successfully.", type = "message")
  })

  filtered_expenses <- reactive({
    df <- expenses_data()
    if (nrow(df) == 0) {
      return(df)
    }
    range <- input$report_range
    if (length(range) != 2 || any(is.na(range))) {
      return(df)
    }
    filtered <- df %>%
      dplyr::filter(Date >= range[1], Date <= range[2])
    if (isTRUE(input$exclude_zero)) {
      filtered <- filtered %>% dplyr::filter(Amount > 0)
    }
    filtered
  })

  category_summary_data <- reactive({
    df <- filtered_expenses()
    if (nrow(df) == 0) {
      return(data.frame())
    }
    summary <- df %>%
      dplyr::group_by(Category) %>%
      dplyr::summarise(
        Total = sum(Amount, na.rm = TRUE),
        Transactions = dplyr::n()
      ) %>%
      dplyr::arrange(dplyr::desc(Total)) %>%
      dplyr::mutate(Percentage = Total / sum(Total))
    summary
  })

  output$category_summary <- renderDT({
    summary <- category_summary_data()
    validate(need(nrow(summary) > 0, "No expenses available for the selected range."))
    datatable(
      summary,
      rownames = FALSE,
      options = list(dom = "ft", pageLength = 10)
    ) %>%
      formatCurrency("Total", currency = "$", interval = 3, mark = ",", digits = 2) %>%
      formatPercentage("Percentage", digits = 1)
  })

  output$category_plot <- renderPlot({
    summary <- category_summary_data()
    validate(need(nrow(summary) > 0, "No data to plot for the selected range."))

    ggplot(summary, aes(x = reorder(Category, Total), y = Total)) +
      geom_col(fill = "#1b9e77") +
      coord_flip() +
      labs(x = "Category", y = "Total spent", title = "Spending by category") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 14)
  })

  output$detailed_table <- renderDT({
    df <- filtered_expenses()
    validate(need(nrow(df) > 0, "No expenses match the selected range."))

    datatable(
      df[order(df$Date, decreasing = TRUE), ],
      rownames = FALSE,
      options = list(pageLength = 15, lengthMenu = c(10, 15, 25, 50))
    ) %>%
      formatCurrency("Amount", currency = "$", interval = 3, mark = ",", digits = 2)
  })
}

shinyApp(ui = ui, server = server)
