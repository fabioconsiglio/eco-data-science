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
library(dplyr)
library(readr)

# Load data (adjust paths to your files)
homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/homicide-rate-unodc.csv")
inequ_data_orig <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")

# Rename the column
colnames(inequ_data_orig)[3] <- "gini_before_tax"

# Define the UI
ui <- fluidPage(
  titlePanel("Gini Coefficient Over the Years"),
  sidebarLayout(
    sidebarPanel(
      # Create a dropdown to select countries
      selectInput(
        inputId = "selected_countries",
        label = "Select Countries:",
        choices = unique(inequ_data_orig$Country),  # Populate with available countries
        selected = c('France', 'Germany', 'Norway', 'Spain', 'Peru', 'Argentina', 'Chile', 'Sub-Saharan Africa (WID)'),
        multiple = TRUE
      )
    ),
    mainPanel(
      # Show the plot
      plotOutput("giniPlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Reactive expression to filter the data based on selected countries
  filtered_data <- reactive({
    inequ_data_orig %>%
      filter(Year >= 1974, Country %in% input$selected_countries)
  })
  
  # Generate the plot
  output$giniPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Year, y = gini_before_tax, color = Country)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Gini Coefficient Over the Years",
        x = "Year",
        y = "Gini Coefficient"
      ) +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

