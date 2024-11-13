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

finance_data_long <- read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/preprocessed/stock_data.csv")  # Load your finance dataset


head(finance_data_long)


# Create lists of measures
inequality_measures <- colnames(inequ_homicide_data)[c(-1,-2,-3,-4)]  # Inequality measures
homicide_measure <- colnames(inequ_homicide_data)[4]  # Homicide rate
stock_indices <- unique(finance_data_long$Index)  # Stock indices
# Define UI for application
ui <- fluidPage(
  titlePanel("Homicide, Inequality, and Financial Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("measure1", "Select First Measure:",
                  choices = list(
                    "Inequality Measures" = inequality_measures,
                    "Homicide Rate" = homicide_measure,
                    "Stock Indices" = stock_indices
                  )
      ),
      selectInput("measure2", "Select Second Measure:",
                  choices = list(
                    "Inequality Measures" = inequality_measures,
                    "Homicide Rate" = homicide_measure,
                    "Stock Indices" = stock_indices
                  )
      ),
      # Always display the country selection input
      selectInput("country", "Select Country:",
                  choices = unique(inequ_homicide_data$country), multiple = TRUE),
      # Add checkbox for world events
      checkboxInput("show_events", "Show World Events", value = FALSE)
    ), 
    
    mainPanel(
      plotlyOutput("rate_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$rate_plot <- renderPlotly({
    req(input$measure1, input$measure2)
    
    measure1 <- input$measure1
    measure2 <- input$measure2
    
    measures_inequ <- c(inequality_measures, homicide_measure)
    measures_stock <- stock_indices
    
    measure1_in_inequ <- measure1 %in% measures_inequ
    measure2_in_inequ <- measure2 %in% measures_inequ
    
    measure1_in_finance <- measure1 %in% measures_stock
    measure2_in_finance <- measure2 %in% measures_stock
    
    # Filter inequ_homicide_data
    if (measure1_in_inequ || measure2_in_inequ) {
      req(input$country)
      filtered_inequ_data <- inequ_homicide_data %>%
        filter(country %in% input$country) %>%
        rename(Date = year_date)
    } else {
      filtered_inequ_data <- data.frame()
    }
    
    # Filter finance_data_long
    if (measure1_in_finance || measure2_in_finance) {
      filtered_finance_data <- finance_data_long %>% filter(Index %in% c(measure1, measure2))
    } else {
      filtered_finance_data <- data.frame()
    }
    
    # Initialize plotly object
    plot <- plot_ly()
    
    # Add measure1
    if (measure1_in_inequ) {
      data1 <- filtered_inequ_data
      for (country in unique(data1$country)) {
        country_data <- data1 %>% filter(country == !!country)
        plot <- plot %>% add_lines(
          x = country_data$Date,
          y = country_data[[measure1]],
          name = paste(measure1, "-", country),
          yaxis = "y1",
          line = list(dash = 'solid')
        )
      }
    } else if (measure1_in_finance) {
      data1 <- filtered_finance_data %>% filter(Index == measure1)
      plot <- plot %>% add_lines(
        x = data1$Date,
        y = data1$Close,
        name = measure1,
        yaxis = "y1",
        line = list(dash = 'solid')
      )
    }
    
    # Add measure2
    if (measure2_in_inequ) {
      data2 <- filtered_inequ_data
      for (country in unique(data2$country)) {
        country_data <- data2 %>% filter(country == !!country)
        plot <- plot %>% add_lines(
          x = country_data$Date,
          y = country_data[[measure2]],
          name = paste(measure2, "-", country),
          yaxis = "y2",
          line = list(dash = 'dash')
        )
      }
    } else if (measure2_in_finance) {
      data2 <- filtered_finance_data %>% filter(Index == measure2)
      plot <- plot %>% add_lines(
        x = data2$Date,
        y = data2$Close,
        name = measure2,
        yaxis = "y2",
        line = list(dash = 'dash')
      )
    }
    
    # Set up the layout with multiple y-axes
    plot <- plot %>%
      layout(
        #title = "Comparison of Selected Measures",
        xaxis = list(title = "Date"),
        yaxis = list(title = measure1, side = "left"),
        yaxis2 = list(title = measure2, overlaying = "y", side = "right")
        #legend = list(x = 5, y = -0.9)
      )
    
    # Add world events if checkbox is selected
    if (input$show_events) {
      # Define the events
      events <- data.frame(
        date = as.Date(c("1991-01-01", "1997-01-01", "2000-01-01", "2008-01-01", "2020-01-01")),
        label = c("Fall of the Soviet Union", "Asian Financial Crisis", "Dotcom Bubble Burst", "Financial Crisis", "COVID-19")
      )
      
      # Add vertical lines for events as shapes
      event_shapes <- lapply(1:nrow(events), function(i) {
        list(
          type = "line",
          x0 = events$date[i],
          x1 = events$date[i],
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "black", dash = "dash")
        )
      })
      
      # Add annotations for events
      event_annotations <- lapply(1:nrow(events), function(i) {
        list(
          x = events$date[i],
          y = 1.05,  # Position slightly above the plot
          xref = "x",
          yref = "paper",
          text = events$label[i],
          showarrow = FALSE,
          xanchor = 'left',
          textangle = -90,
          font = list(color = "black", size = 10)
        )
      })
      
      # Add shapes and annotations to the plot layout
      plot <- plot %>%
        layout(
          shapes = event_shapes,
          annotations = event_annotations
        )
    }
    
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
