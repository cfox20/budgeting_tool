library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

# Minimal in-memory stores ----------------------------------------------------

empty_expenses <- tibble::tibble(
  Date = as.Date(character()),
  Description = character(),
  Category = character(),
  Amount = numeric()
)

empty_budgets <- tibble::tibble(
  Category = character(),
  Limit = numeric()
)

# User interface --------------------------------------------------------------

ui <- navbarPage(
  title = "Household Budgeting",
  tabPanel(
    "Expenses",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Add an expense"),
          dateInput("date", "Date", value = Sys.Date()),
          textInput("description", "Description"),
          textInput("category", "Category"),
          numericInput("amount", "Amount", value = NA, min = 0, step = 0.01),
          actionButton("add_expense", "Add expense", class = "btn-primary"),
          br(),
          br(),
          strong("Totals"),
          textOutput("totals", inline = FALSE)
        ),
        column(
          width = 8,
          h3("Recent expenses"),
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
          h3("Monthly planning"),
          numericInput("income", "Monthly income", value = NA, min = 0, step = 100),
          actionButton("set_income", "Save income", class = "btn-secondary"),
          br(),
          br(),
          h4("Add category budget"),
          textInput("budget_category", "Category"),
          numericInput("budget_limit", "Budget limit", value = NA, min = 0, step = 10),
          actionButton("add_budget", "Add budget", class = "btn-primary")
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
          h3("Spending vs budget"),
          DTOutput("report_table"),
          br(),
          h3("Spending by category"),
          plotOutput("category_plot", height = "350px"),
          br(),
          h3("Category details"),
          DTOutput("category_table")
        )
      )
    )
  )
)

# Server logic ----------------------------------------------------------------

server <- function(input, output, session) {
  expenses <- reactiveVal(empty_expenses)
  budgets <- reactiveVal(empty_budgets)
  monthly_income <- reactiveVal(NA_real_)

  observeEvent(input$add_expense, {
    description <- trimws(input$description)
    category <- trimws(input$category)

    validate(
      need(!is.null(input$date) && !is.na(input$date), "Please supply a date."),
      need(nzchar(description), "Describe the expense."),
      need(nzchar(category), "Provide a category."),
      need(!is.null(input$amount) && !is.na(input$amount) && input$amount > 0, "Enter a positive amount.")
    )

    new_entry <- tibble::tibble(
      Date = as.Date(input$date),
      Description = description,
      Category = category,
      Amount = as.numeric(input$amount)
    )

    expenses(bind_rows(expenses(), new_entry))

    updateTextInput(session, "description", value = "")
    updateTextInput(session, "category", value = "")
    updateNumericInput(session, "amount", value = NA)
  })

  observeEvent(input$add_budget, {
    category <- trimws(input$budget_category)

    validate(
      need(nzchar(category), "Provide a category name."),
      need(!is.null(input$budget_limit) && !is.na(input$budget_limit) && input$budget_limit > 0, "Enter a positive limit.")
    )

    new_budget <- tibble::tibble(
      Category = category,
      Limit = as.numeric(input$budget_limit)
    )

    current <- budgets()
    existing_index <- match(tolower(new_budget$Category), tolower(current$Category))

    if (!is.na(existing_index)) {
      current$Limit[existing_index] <- new_budget$Limit
      budgets(current)
    } else {
      budgets(bind_rows(current, new_budget))
    }

    updateTextInput(session, "budget_category", value = "")
    updateNumericInput(session, "budget_limit", value = NA)
  })

  observeEvent(input$set_income, {
    validate(need(!is.null(input$income) && !is.na(input$income) && input$income >= 0, "Enter a non-negative income."))
    monthly_income(as.numeric(input$income))
  })

  output$totals <- renderText({
    df <- expenses()
    total <- sum(df$Amount, na.rm = TRUE)
    paste0("Recorded expenses: ", nrow(df), " | Total spent: ", scales::dollar(total))
  })

  output$expense_table <- renderDT({
    datatable(
      expenses(),
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency("Amount", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  output$budget_table <- renderDT({
    data <- budgets()
    validate(need(nrow(data) > 0, "Add budgets to track your plan."))

    datatable(
      data,
      rownames = FALSE,
      options = list(dom = "ft", pageLength = 10)
    ) %>%
      formatCurrency("Limit", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  output$income_summary <- renderUI({
    income <- monthly_income()
    budget_total <- sum(budgets()$Limit, na.rm = TRUE)
    income_display <- if (is.na(income)) "Not set" else scales::dollar(income)
    unallocated <- if (is.na(income)) "--" else scales::dollar(income - budget_total)

    tags$p(
      strong("Monthly income:"),
      income_display,
      tags$br(),
      strong("Budgeted:"),
      scales::dollar(budget_total),
      tags$br(),
      strong("Unallocated:"),
      unallocated
    )
  })

  category_summary <- reactive({
    df <- expenses()
    if (nrow(df) == 0) {
      return(tibble::tibble(Category = character(), Total = numeric(), Transactions = integer()))
    }

    df %>%
      group_by(Category) %>%
      summarise(
        Total = sum(Amount, na.rm = TRUE),
        Transactions = dplyr::n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total))
  })

  report_data <- reactive({
    category_summary() %>%
      full_join(budgets(), by = "Category") %>%
      mutate(
        Total = tidyr::replace_na(Total, 0),
        Transactions = tidyr::replace_na(Transactions, 0L),
        Limit = tidyr::replace_na(Limit, 0),
        Remaining = Limit - Total,
        Status = case_when(
          Limit == 0 & Total == 0 ~ "No budget",
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
      options = list(dom = "ft", pageLength = 10)
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
      options = list(dom = "ft", pageLength = 10)
    ) %>%
      formatCurrency("Total", currency = "$", interval = 3, mark = ",", digits = 2)
  })

  output$category_plot <- renderPlot({
    summary <- category_summary()
    validate(need(nrow(summary) > 0, "Add expenses to see the plot."))

    ggplot(summary, aes(x = reorder(Category, Total), y = Total)) +
      geom_col(fill = "#1b9e77") +
      coord_flip() +
      labs(x = "Category", y = "Total spent", title = "Spending by category") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
