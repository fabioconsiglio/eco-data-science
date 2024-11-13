#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

# Load data
inequ_homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/preprocessed/inequ_homicide_data.csv")

# Define UI for application
ui <- fluidPage(
  titlePanel("Homicide and Inequality Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("inequality_measure", "Select Inequality Measure:",
                  choices = colnames(inequ_homicide_data)[c(-1,-2,-3,-4)]),
      selectInput("homicide_measure", "Add Homicide Rate:",
                  choices = colnames(inequ_homicide_data)[4]),
      selectInput("country", "Select Country:",
                  choices = unique(inequ_homicide_data$country), multiple = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("rate_plot")  # Use plotlyOutput for interactivity
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$rate_plot <- renderPlotly({
    req(input$country, input$inequality_measure, input$homicide_measure)
    
    # Filter data by selected countries
    filtered_data <- inequ_homicide_data %>%
      filter(country %in% input$country)
    
    # Create a ggplot
    p <- ggplot() +
      # Inequality measure line
      geom_line(data = filtered_data, aes(x = year, y = .data[[input$inequality_measure]], color = country), linetype = "solid") +
      
      # Homicide rate line with a different line type
      geom_line(data = filtered_data, aes(x = year, y = .data[[input$homicide_measure]], color = country), linetype = "dashed") +
      
      labs(x = "Year", y = "Rates") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      ggtitle("Yearly Trends for Selected Inequality Measure and Homicide Rate by Country") +
      
      # Secondary y-axis for the homicide measure
      scale_y_continuous(
        name = input$inequality_measure,
        sec.axis = sec_axis(~., name = input$homicide_measure)
      )
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p, tooltip = c("x", "y", "color"))  # Tooltip shows x (year), y (rate), and country
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


