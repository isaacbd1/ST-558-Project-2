library(shiny)

source("us_superstore_data.xls", encoding="utf-8")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cat_vars", "Select at least 2 categorical variables to subset from:", 
                         choices = c("Ship Mode", "Segment", "Region", "State", "City", "Category", "Sub-Category"),
                         selected = NULL),
      conditionalPanel(
        condition = '$.inArray("Ship Mode", input.cat_vars) > -1',
        checkboxGroupInput("ship_modes", "Ship Mode Options", choices = append(as.vector(sort(unique((us_superstore_data$`Ship Mode`)))), "All of the above"),
        selected = "All of the above")
      ),
      conditionalPanel(
        condition = '$.inArray("Segment", input.cat_vars) > -1',
        checkboxGroupInput("segments", "Segment Options", choices = append(as.vector(sort(unique((us_superstore_data$Segment)))), "All of the above"),
                           selected = "All of the above")
      ),
      conditionalPanel(
        condition = '$.inArray("Region", input.cat_vars) > -1',
        checkboxGroupInput("regions", "Region Options", choices = append(as.vector(sort(unique((us_superstore_data$Region)))), "All of the above"),
                           selected = "All of the above")
      ),
      conditionalPanel(
        condition = '$.inArray("State", input.cat_vars) > -1',
        selectInput("states", "State Options", choices = append(as.vector(sort(unique((us_superstore_data$State)))), "All states"),
                           selected = "All states")
      ),
      conditionalPanel(
        condition = '$.inArray("City", input.cat_vars) > -1',
        selectInput("cities", "City Options", choices = append(as.vector(sort(unique((us_superstore_data$City)))), "All cities"),
                    selected = "All cities")
      ),
      conditionalPanel(
        condition = '$.inArray("Category", input.cat_vars) > -1',
        checkboxGroupInput("categories", "Category Options", choices = append(as.vector(sort(unique((us_superstore_data$Category)))), "All of the above"),
                    selected = "All of the above")
      ),
      conditionalPanel(
        condition = '$.inArray("Sub-Category", input.cat_vars) > -1',
        selectInput("sub_categories", "Sub-Category Options", choices = append(as.vector(sort(unique((us_superstore_data$`Sub-Category`)))), "All sub-categories"),
                           selected = "All sub-categories")
      ),
      radioButtons("num_var1", "Select a numeric variable:", choices = c("Sales", "Quantity", "Discount", "Profit"), selected = character(0)), 
      conditionalPanel(
        condition = 'input.num_var1 == "Sales"',
        sliderInput("sales_range", "Sales Range", min = floor(min(us_superstore_data$Sales)), max = ceiling(max(us_superstore_data$Sales)), value = c(5,55), step = 5)
      ),
      conditionalPanel(
        condition = 'input.num_var1 == "Quantity"',
        sliderInput("quantity_range", "Quantity Range", min = min(us_superstore_data$Quantity), max = max(us_superstore_data$Quantity), value = c(1,3), step = 1)
      ),
      conditionalPanel(
        condition = 'input.num_var1 == "Discount"',
        sliderInput("discount_range", "Discount Range", min = min(us_superstore_data$Discount), max = max(us_superstore_data$Discount), value = c(0.1,0.2), step = 0.1)
      ),
      conditionalPanel(
        condition = 'input.num_var1 == "Profit"',
        sliderInput("profit_range", "Profit Range", min = floor(min(us_superstore_data$Profit)), max = ceiling(max(us_superstore_data$Profit)), value = c(1,9), step = 1)
      ),
      radioButtons("num_var2", "Select a second numeric variable:", choices = c("Sales", "Quantity", "Discount", "Profit"), selected = character(0)), 
      conditionalPanel(
        condition = 'input.num_var2 == "Sales"',
        sliderInput("sales_range", "Sales Range", min = floor(min(us_superstore_data$Sales)), max = ceiling(max(us_superstore_data$Sales)), value = c(5,55), step = 5)
      ),
      conditionalPanel(
        condition = 'input.num_var2 == "Quantity"',
        sliderInput("quantity_range", "Quantity Range", min = min(us_superstore_data$Quantity), max = max(us_superstore_data$Quantity), value = c(1,3), step = 1)
      ),
      conditionalPanel(
        condition = 'input.num_var2 == "Discount"',
        sliderInput("discount_range", "Discount Range", min = min(us_superstore_data$Discount), max = max(us_superstore_data$Discount), value = c(0.1,0.2), step = 0.1)
      ),
      conditionalPanel(
        condition = 'input.num_var2 == "Profit"',
        sliderInput("profit_range", "Profit Range", min = floor(min(us_superstore_data$Profit)), max = ceiling(max(us_superstore_data$Profit)), value = c(1,9), step = 1)
      ),
      actionButton("subset_data", "Subset Data Accordingly")
    ),
    mainPanel(
      
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
