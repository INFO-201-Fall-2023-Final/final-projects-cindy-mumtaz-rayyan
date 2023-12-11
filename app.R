# Load necessary libraries
library(shiny)
library(ggplot2)
library(DT)

# Load your dataset
# Replace 'your_dataset.csv' with your actual dataset file
df <- read.csv('MergedData.csv')

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Data Storytelling with Shiny"),
  
  # Main panel with scrolling layout
  mainPanel(
    # Text section
    tags$div(
      style = "height: 300px; overflow-y: auto; padding: 20px;",
      tags$p("Write your filler words and introductory text here."),
      # Add more text or content as needed
    ),
    
    # Scatter plot section
    plotOutput("scatter_plot"),
    
    # Dropdown selection box
    selectInput("column_selector", "Select a Column", choices = colnames(df)),
    
    # Tabs for plot and table
    tabsetPanel(
      tabPanel("Plot", plotOutput("selected_plot")),
      tabPanel("Table", DTOutput("selected_table"))
    )
  )
)

# Define server logic, change aes, general plot
server <- function(input, output) {
  output$scatter_plot <- renderPlot({
    ggplot(df, aes(x = `ELA..Level.4`, y = Category, color = Category)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Scatter Plot: ELA Level 4 vs. Category",
           x = "ELA Level 4",
           y = "Category")
  })
  
  # Render selected plot based on dropdown choice, change aes, plot for every
  output$selected_plot <- renderPlot({
    if (!is.null(input$column_selector)) {
      ggplot(df, aes_string(x = input$column_selector, y = "Category")) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = paste("Scatter Plot for", input$column_selector),
             x = input$column_selector,
             y = "Category")
    }
  })
  
  # Render selected table based on dropdown choice
  output$selected_table <- renderDT({
    if (!is.null(input$column_selector)) {
      datatable(df[, c(input$column_selector, "Category")])
    }
  })
}

# Run the application
shinyApp(ui, server)

