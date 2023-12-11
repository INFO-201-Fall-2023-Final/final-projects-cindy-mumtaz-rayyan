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
      style = "height: 200px; padding: 20px; width: 1230px;",
      tags$h2("Overview & Its Importance:"),
      tags$p(
        "The data analyzed in this project is sourced from the New York State Government School Attendance and Diversity Report, covering the years 2018 to 2022. This dataset offers a comprehensive examination of class attendance and identity factors, 
        providing a lens into the intricate web of influences shaping students' academic journeys.",
        "Acknowledging the influence of demographic and social factors on academic performance is crucial. According to the 2012 student achievement factors published by the Department of Education (",
        tags$a(href = "https://files.eric.ed.gov/fulltext/ED568687.pdf", "source"),
        ").", "Understanding the determinants of academic success is crucial, and this dataset delves into critical facets such as social and moral development, developmental differences, readiness for skill development, and socioeconomic disparities. 
        In aligning with the insights shared by the Department of Education in their 2012 publication on student achievement factors, our exploration aims to unravel the intricate interplay of these elements. 
        Our goal is to equip educators, policymakers, and parents, with the insights necessary to craft more nuanced and effective education plans for students. 
        Moreover, the analysis serves as a powerful tool to identify pain points, particularly in underfunded school districts, providing a roadmap for targeted interventions and resource allocations."
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
        "In summary, the dataset reveals that attendance rates are influenced by various factors, including the students' surroundings, socioeconomic conditions, and educational challenges. 
        These factors may contribute to a deprioritization of education or internal barriers hindering school attendance. Identifying such outliers is crucial for assessing individual academic needs, allowing for increased support and the development of inclusive institutions. 
        Regardless of physical identity, socioeconomic status, financial circumstances, or language proficiency, it is essential to create an environment where every student can thrive and find motivation in their educational pursuits. 
        The data highlights specific pain points and areas for focus, emphasizing the importance of promoting inclusivity and supporting students, irrespective of their backgrounds. 
        It's important to note that not all students with marginalized identities exhibit negative school performance; outliers, as indicated in the dataset, exist."
      ),
      tags$h3("Dataset Overview:"),
      tags$p(
        "This dataset provides comprehensive information on student demographics, encompassing categories such as race/ethnicity, intellectual disabilities, English-language learners, and economic indicators, including whether students qualify for free or reduced lunch.
        Through an analysis of this merged dataset, we aim to investigate the potential impact of these demographic factors on two crucial aspects of student life: attendance and academic performance. 
        By delving into attendance patterns and academic outcomes in relation to these demographic variables, we seek to discern any discernible trends or correlations. 
        This exploration will contribute valuable insights into understanding how various student characteristics may influence their commitment to attending classes and, subsequently, their performance in academic pursuits.
        The study will not only shed light on the potential disparities in attendance and academic achievements across different demographic groups but also offer an opportunity to identify areas where targeted interventions and support mechanisms can be implemented. 
        The overarching goal is to foster a more equitable and inclusive educational environment that addresses the diverse needs of students, ensuring that factors such as race, ethnicity, intellectual abilities, language proficiency, and economic status do not become barriers to accessing quality education."
      ),
      tags$h3("Takeaways:"),
      tags$p(
        "A key takeaway from our analysis is the recognition that a holistic approach, integrating both data and research, is crucial for identifying and implementing successful interventions. 
        This acknowledgement underscores the significance of leveraging comprehensive information to tailor effective strategies that cater to the individual needs of students. 
        The amalgamation of data-driven insights and informed research equips educators and policymakers with the tools needed to create targeted interventions, fostering an educational environment where every child has the opportunity to thrive.
        From our research, a notable observation emerged, revealing that students grappling with disabilities, homelessness, and financial hardship exhibit lower attendance rates compared to their counterparts who do not face these challenges. 
        This stark contrast underscores the profound impact of socio-economic factors and diverse life circumstances on students' ability to regularly attend school."
      ),
      tags$h3("Data Information:"),
      tags$p(
        "Data Creation Range: 2018-2022 Created By: City and State of New York Government
        Content: School Attendance and Diversity Report File
        Source: data.gov
        2018-2019 Diversity Reports, Special Programs: https://catalog.data.gov/dataset/2014-2015- diversity-report-k-8-grades-9-12-district-schools- special-programs-diversity-eff
        2021-2022 School Attendance by Student Group: https://catalog.data.gov/dataset/school- attendance-by-student-group-and-district-2021- 2022"
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

