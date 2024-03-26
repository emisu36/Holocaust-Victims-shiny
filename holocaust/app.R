#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(DT) # for interactive tables

# Define the dataset
data <- data.frame(
  Nationality_Category = c("Jews", "Poles", "Other groups", "Roma (Gypsies)", "Soviet POWs"),
  Number_of_Deportees = c(1100000, 140000, 25000, 23000, 15000),
  Percentage_of_Total_Deportees = c(85, 10.8, 1.9, 1.6, 1.2),
  Number_of_Victims = c(1000000, 70000, 12000, 21000, 14000),
  Percentage_of_Murdered = c(90, 46, 48, 91.3, 93),
  Percentage_of_All_Victims = c(91, 5.8, 1, 1.7, 1.3)
)

# Define UI
ui <- fluidPage(
  titlePanel("Holocaust Victims at Auschwitz Concentration Camp"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category/Nationality:",
                  choices = c("Jews", "Poles", "Other groups", "Roma (Gypsies)", "Soviet POWs"))
    ),
    mainPanel(
      plotOutput("barplot"),
      br(),
      plotOutput("plot"), # Add a plot output for the given barplot
      br(),
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render the given barplot
  output$barplot <- renderPlot({
    ggplot(data, aes(x = Nationality_Category)) +
      geom_bar(aes(y = Number_of_Deportees, fill = "Number of Deportees"), stat = "identity") +
      geom_bar(aes(y = Number_of_Victims, fill = "Number of Victims"), stat = "identity") +
      ylab("Count") +
      ggtitle("Number of Deportees and Victims by Nationality/Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("Number of Deportees" = "blue", "Number of Victims" = "red"), 
                        guide = guide_legend(title = "Category"))
  })
  # Generate plot based on selected category
  output$plot <- renderPlot({
    category_data <- data[data$Nationality_Category == input$category, ]
    
    ggplot(category_data, aes(x = "", fill = Nationality_Category)) +
      geom_bar(aes(y = Number_of_Deportees), stat = "identity", fill = "blue", width = 0.5) +
      geom_bar(aes(y = Number_of_Victims), stat = "identity", fill = "red", width = 0.5) +
      coord_polar(theta = "y") +
      labs(title = paste("Amount of Victims (Red) over Deportees (Blue) for", input$category),
           x = NULL, y = NULL,
           fill = "Category") +
      scale_fill_manual(values = c("blue", "red")) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom")
  })
  
  # Generate table based on selected category
  output$table <- renderDT({
    datatable(data[data$Nationality_Category == input$category, ], 
              options = list(lengthChange = FALSE, searching = FALSE))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
