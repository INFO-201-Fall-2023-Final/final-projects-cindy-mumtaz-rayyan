#INFO 201 Final Project: Created by Mumtaz, Cindy, Rayyan
#INFO BD: Julia Deeb-Swihart
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

df <- read.csv('MergedData.csv')


ui <- fluidPage(
  titlePanel("Topic: The Influence of Demographic and Social Factors on Academic Performance/Student Outcomes"),
  
  mainPanel(
    tags$div(
      style = "height: 200px; padding: 20px; width: 1200px;",
      tags$h2("Overview & Its Importance:"),
      tags$p(
        "The data analyzed in this project is sourced from the New York State Government School Attendance and Diversity Report, covering the years 2018 to 2022. This report reviews class attendance and identity factors.",
        "Understanding the influence of demographic and social factors on academic performance is crucial. According to the 2012 student achievement factors published by the Department of Education (",
        tags$a(href = "https://files.eric.ed.gov/fulltext/ED568687.pdf", "source"),
        "), the primary determinants of academic success include Social and Moral development, Developmental differences, readiness for skills development, and Socioeconomic disparities.",
        "Visualizing this data is essential for stakeholders such as educators, policymakers, and parents. It enables them to create better education plans for students and identify pain points in underfunded school districts.",
        "Insights from this analysis can aid in funding prioritization, guiding decisions on after-school clubs, additional tutoring support, mental and emotional health counseling, and allocating financial resources to schools."
      )
    ),
    br(),
    br(),
    tags$h3("Attendance Rate by Category"),
    
    
    plotOutput("scatter_plot"),
    br(),
    br(),
    br(),
    
    tags$h3("Choose a column to see the students attendance per column."),
    
    
    selectInput("column_selector", "Select a Column", choices = colnames(df)),
    br(),
    br(),
    
    tabsetPanel(
      tabPanel("Plot", plotOutput("selected_plot")),
      tabPanel("Table", DTOutput("selected_table"))
    ),
    br(),
    br(),
    
    tags$div(
      style = "padding: 20px; width: 1200px;",
      tags$h3("Conclusion/Summary:"),
      tags$p(
        "The data concludes that consistent attendance and overall academic performance were affected by socioeconomic indicators like race, ethnicity, and the financial status of students.",
        "Additionally, students' attendance and performance were impacted by cognitive ability and cultural barriers like English not being their mother tongue.",
        "This does not mean all students who hold marginalized identities have negative school performance. Outliers exist, as shown in our dataset."
      ),
      tags$h3("Dataset Overview:"),
      tags$p(
        "This dataset reports student demographics based on race/ethnicity, intellectual disabilities, English-language learners, and economic indicators such as free or reduced lunch qualifiers.",
        "Using this merged dataset, we determined if those factors affect a student's attendance and academic performance."
      ),
      tags$h3("Takeaways:"),
      tags$p(
        "Throughout this process, we learned the many factors that contributed to children’s academic success and how various school districts handle it.",
        "We also discussed how to identify and support outliers in the dataset. Finally, we concluded that a combination of data and research is necessary to identify successful interventions."
      )
    )
  )
)

server <- function(input, output) {
  output$scatter_plot <- renderPlot({
    ggplot(df, aes(x = X2021.2022.attendance.rate...year.to.date, y = Category, color = Category)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Scatter Plot: Attendance Rate vs. Category",
           x = "Attendance Rate 2021 - 2022",
           y = "Category")+
      theme_minimal() +  
      theme(
        text = element_text(family = "Arial", color = "darkblue", size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)
      )
  })
  
  output$selected_plot <- renderPlot({
    if (!is.null(input$column_selector)) {
      ggplot(df, aes_string(x = input$column_selector, y = "X2021.2022.attendance.rate...year.to.date", color = "Category")) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = paste("Scatter Plot for", input$column_selector),
             x = input$column_selector,
             y = "Attendance Rata 2021 - 2022")+
    theme_minimal() +  
    theme(
      text = element_text(family = "Arial", color = "darkblue", size = 14),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12)
    )
    }
  })
  
  output$selected_table <- renderDT({
    if (!is.null(input$column_selector)) {
      datatable(df[, c(input$column_selector, "X2021.2022.attendance.rate...year.to.date")])
    }
  })
}

shinyApp(ui, server)

