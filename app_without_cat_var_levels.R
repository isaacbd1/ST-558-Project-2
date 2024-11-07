library(shiny)

source("us_superstore_data.xls", encoding="utf-8")

ui <- fluidPage(
  titlePanel("US Superstore Data App"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cat_vars", "Select at least 2 categorical variables to subset from:", 
                         choices = c("Ship Mode", "Segment", "Region", "State", "City", "Category", "Sub-Category"),
                         selected = NULL),
      radioButtons("num_var1", "Select a numeric variable:", choices = c("Sales", "Quantity", "Discount", "Profit"), selected = character(0)), 
      conditionalPanel(
        condition = 'input.num_var1 == "Sales"',
        sliderInput("sales_range1", "Sales Range", min = floor(min(us_superstore_data$Sales)), max = ceiling(max(us_superstore_data$Sales)), value = c(5,55), step = 5)
      ),
      conditionalPanel(
        condition = 'input.num_var1 == "Quantity"',
        sliderInput("quantity_range1", "Quantity Range", min = min(us_superstore_data$Quantity), max = max(us_superstore_data$Quantity), value = c(1,3), step = 1)
      ),
      conditionalPanel(
        condition = 'input.num_var1 == "Discount"',
        sliderInput("discount_range1", "Discount Range", min = min(us_superstore_data$Discount), max = max(us_superstore_data$Discount), value = c(0.1,0.2), step = 0.1)
      ),
      conditionalPanel(
        condition = 'input.num_var1 == "Profit"',
        sliderInput("profit_range1", "Profit Range", min = floor(min(us_superstore_data$Profit)), max = ceiling(max(us_superstore_data$Profit)), value = c(1,9), step = 1)
      ),
      radioButtons("num_var2", "Select a second numeric variable:", choices = c("Sales", "Quantity", "Discount", "Profit"), selected = character(0)), 
      conditionalPanel(
        condition = 'input.num_var2 == "Sales"',
        sliderInput("sales_range2", "Sales Range", min = floor(min(us_superstore_data$Sales)), max = ceiling(max(us_superstore_data$Sales)), value = c(5,55), step = 5)
      ),
      conditionalPanel(
        condition = 'input.num_var2 == "Quantity"',
        sliderInput("quantity_range2", "Quantity Range", min = min(us_superstore_data$Quantity), max = max(us_superstore_data$Quantity), value = c(1,3), step = 1)
      ),
      conditionalPanel(
        condition = 'input.num_var2 == "Discount"',
        sliderInput("discount_range2", "Discount Range", min = min(us_superstore_data$Discount), max = max(us_superstore_data$Discount), value = c(0.1,0.2), step = 0.1)
      ),
      conditionalPanel(
        condition = 'input.num_var2 == "Profit"',
        sliderInput("profit_range2", "Profit Range", min = floor(min(us_superstore_data$Profit)), max = ceiling(max(us_superstore_data$Profit)), value = c(1,9), step = 1)
      ),
      actionButton("subset_data", "Subset Data Accordingly")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("About"),
        tabPanel("Data Download",
                 DT::dataTableOutput("subsetted_data_table"),
                 downloadButton("subsetted_data_file", "Download")
                 ),
        tabPanel("Data Exploration",
                 selectInput("summaries", "Statistical Summaries", choices = c("Categorical Summary", "Numeric Summary", "Both"), selected = NULL),
                 DT::dataTableOutput("contingency_table"),
                 DT::dataTableOutput("numeric_statistics")
      )
    )
  )
)

server <- function(input, output) {
  
  subsetted_data <- eventReactive(input$subset_data, {
    if (input$num_var1 == input$num_var2) {
      validate("You must select two distinct numeric variables!")
    }
    else if (input$num_var1 == "Sales") {
      if (input$num_var2 == "Quantity") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Sales >= input$sales_range1[1] & Sales <= input$sales_range1[2]) |>
          filter(Quantity >= input$quantity_range2[1] & Quantity <= input$quantity_range2[2])
      }
    }
    else if (input$num_var1 == "Sales") {
      if (input$num_var2 == "Discount") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Sales >= input$sales_range1[1] & Sales <= input$sales_range1[2]) |>
          filter(Discount >= input$discount_range2[1] & Discount <= input$discount_range2[2])
      }
    }
    else if (input$num_var1 == "Sales") {
      if (input$num_var2 == "Profit") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Sales >= input$sales_range1[1] & Sales <= input$sales_range1[2]) |>
          filter(Profit >= input$profit_range2[1] & Profit <= input$profit_range2[2])
      }
    }
    else if (input$num_var1 == "Quantity") {
      if (input$num_var2 == "Sales") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Quantity >= input$quantity_range1[1] & Quantity <= input$quantity_range1[2]) |>
          filter(Sales >= input$sales_range2[1] & Sales <= input$sales_range2[2])
      }
    }
    else if (input$num_var1 == "Quantity") {
      if (input$num_var2 == "Discount") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Quantity >= input$quantity_range1[1] & Quantity <= input$quantity_range1[2]) |>
          filter(Discount >= input$discount_range2[1] & Discount <= input$discount_range2[2])
      }
    }
    else if (input$num_var1 == "Quantity") {
      if (input$num_var2 == "Profit") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Quantity >= input$quantity_range1[1] & Quantity <= input$quantity_range1[2]) |>
          filter(Profit >= input$profit_range2[1] & Profit <= input$profit_range2[2])
      }
    }
    else if (input$num_var1 == "Discount") {
      if (input$num_var2 == "Sales") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Discount >= input$discount_range1[1] & Discount <= input$discount_range1[2]) |>
          filter(Sales >= input$sales_range2[1] & Sales <= input$sales_range2[2])
      }
    }
    else if (input$num_var1 == "Discount") {
      if (input$num_var2 == "Quantity") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Discount >= input$discount_range1[1] & Discount <= input$discount_range1[2]) |>
          filter(Quantity >= input$quantity_range2[1] & Quantity <= input$quantity_range2[2])
      }
    }
    else if (input$num_var1 == "Discount") {
      if (input$num_var2 == "Profit") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Discount >= input$discount_range1[1] & Discount <= input$discount_range1[2]) |>
          filter(Profit >= input$profit_range2[1] & Profit <= input$profit_range2[2])
      }
    }
    else if (input$num_var1 == "Profit") {
      if (input$num_var2 == "Sales") {
        us_superstore_data |> 
          select(!!sym(input$cat_vars), !!sym(input$num_var1), !!sym(input$num_var2)) |>
          filter(Profit >= input$profit_range1[1] & Profit <= input$profit_range1[2]) |>
          filter(Sales >= input$sales_range2[1] & Sales <= input$sales_range2[2])
      }
    }
    else if (input$num_var1 == "Profit") {
      if (input$num_var2 == "Quantity") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Profit >= input$profit_range1[1] & Profit <= input$profit_range1[2]) |>
          filter(Quantity >= input$quantity_range2[1] & Quantity <= input$quantity_range2[2])
      }
    }
    else if (input$num_var1 == "Profit") {
      if (input$num_var2 == "Discount") {
        us_superstore_data |> 
          select(input$cat_vars, input$num_var1, input$num_var2) |>
          filter(Profit >= input$profit_range1[1] & Profit <= input$profit_range1[2]) |>
          filter(Discount >= input$discount_range2[1] & Discount <= input$discount_range2[2])
      }
    }
    else {
      us_superstore_data
    }
  })
  
  output$subsetted_data_table <- DT::renderDataTable({
    subsetted_data()
  })
  
  output$contingency_table <- DT::renderDataTable({
    selected_cat_vars <- c(),
    for (cat_var in input$cat_vars) {
      selected_cat_vars <- append(selected_cat_vars, cat_var)
      table(subsetted_data()$selected_cat_vars)
    }
  })
  
  output$subsetted_data_file <- downloadHandler(
    function(){"subsetted_data.xls"}, 
    function(file_name){write_excel_csv(subsetted_data(), file_name)}
  )

}

shinyApp(ui = ui, server = server)













