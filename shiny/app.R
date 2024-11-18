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
library(plotly)
library(readr)
####
### Data import
####
inequ_homicide_data <- read_csv("inequ_homicide_data.csv")

finance_data_long <- read_csv("stock_data.csv")  # Load your finance dataset


head(finance_data_long)


# Create lists of measures
inequality_measures <- colnames(inequ_homicide_data)[c(-1,-2,-3,-4,-10)]  # Inequality measures
homicide_measure <- colnames(inequ_homicide_data)[4]  # Homicide rate
stock_indices <- unique(finance_data_long$Index)  # Stock indices

# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        overflow-x: hidden; /* Prevent horizontal scrolling */
      }
      .fixed-sidebar {
        position: fixed;
        top: 0;
        left: 0;
        width: 20%; /* Sidebar width */
        height: 100%;
        overflow-y: auto;
        background-color: #a3d0e4 ; /* Optional: Background color */
        padding: 20px; /* Add padding for better spacing */
        z-index: 1000; /* Ensure it stays on top */
        box-shadow: 2px 0px 5px rgba(0, 0, 0, 0.2); /* Subtle shadow */
      }
      .main-content {
        margin-left: 25%; /* Space for sidebar */
        padding: 20px;
        width: 100%; /* Remaining width for main content */
      }
      .app-title {
        margin-left: 10%; /* Match the sidebar width for alignment */
        padding: 10px 0; /* Add some vertical spacing */
        font-size: 26px; /* Optional: Adjust font size for emphasis */
        font-weight: bold; /* Optional: Make the title bold */
        text-align: center; /* Align title to the left */
      }
      .select-input {
        width: 100%; /* Ensure full width for inputs */
      }
      .action-buttons {
        margin-top: 10px; /* Add spacing between the buttons */
        display: flex;
        flex-direction: column; /* Stack buttons vertically */
        gap: 10px; /* Add spacing between buttons */
      }
      .shiny-output-error-validation {
        color: red; /* Highlight validation errors */
      }
    "))
  ),
  div(class = "app-title", "Comparing Trends in Inequality, Homicide, and Financial Development"),
  fluidRow(
    div(
      class = "fixed-sidebar",
      div(class = "select-input",
          selectInput("measure1", "Select First Measure:",
                      choices = list(
                        "Inequality Measures" = inequality_measures,
                        "Homicide Rate" = list("Homicide Rate per 100k inhabitants" = "Homicide Rate"),
                        "Stock Indices" = stock_indices
                      ), selected = "Gini Coefficient" # Default selection
          )
      ),
      div(class = "select-input",
          selectInput("measure2", "Select Second Measure:",
                      choices = list(
                        "Inequality Measures" = inequality_measures,
                        "Homicide Rate" = list("Homicide Rate per 100k inhabitants" = "Homicide Rate"),
                        "Stock Indices" = stock_indices
                      ), selected = "Palma Ratio" 
          )
      ),
      div(class = "select-input",
          selectInput("country", "Select Country:",
                      choices = unique(inequ_homicide_data$country), 
                      selected = c("United States", "Norway"), 
                      multiple = TRUE)
      ),
      checkboxInput("show_events", "Show World Events", value = FALSE),
      div(class = "action-buttons",
          actionButton("random_select", "Randomly Select Parameters"), 
          downloadButton("downloadData", "Download Data as .csv")
      )
    ),
    div(
      class = "main-content",
      mainPanel(
        plotlyOutput("rate_plot", height = "650"),  # Larger visualization
        includeHTML("description.html")
      )
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  # Download handler for exporting data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      # Determine if selected measures are from inequality/homicide or finance data
      measures_inequ <- c(inequality_measures, homicide_measure)
      measures_stock <- stock_indices
      
      # If both measures are from the inequality/homicide data
      if (input$measure1 %in% measures_inequ && input$measure2 %in% measures_inequ) {
        filtered_data <- inequ_homicide_data %>%
          filter(country %in% input$country) %>%
          select(year_date, country, 
                 !!input$measure1 := all_of(input$measure1), 
                 !!input$measure2 := all_of(input$measure2))
        
        # If both measures are from the finance data
      } else if (input$measure1 %in% measures_stock && input$measure2 %in% measures_stock) {
        filtered_data <- finance_data_long %>%
          filter(Index %in% c(input$measure1, input$measure2)) %>%
          select(Date, Index, Close) %>%
          pivot_wider(names_from = Index, values_from = Close)
        
        # If one measure from each data set, merge them by date otherwise use one data set
      } else {
        # Filter inequality/homicide data based on selected country and measure
        filtered_inequ_data <- inequ_homicide_data %>%
          filter(country %in% input$country) %>%
          select(year_date, country, 
                 !!input$measure1 := all_of(input$measure1))
        
        # Filter finance data based on selected stock measure
        filtered_finance_data <- finance_data_long %>%
          filter(Index == input$measure2) %>%
          select(Date, Close) %>%
          rename(year_date = Date, !!input$measure2 := Close)
        
        # Merge both data sets on the date column
        filtered_data <- merge(filtered_inequ_data, filtered_finance_data, by = "year_date", all = TRUE)
      }
      
      # Write the final filtered data to CSV
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
    
    # Observe random selection button
    observeEvent(input$random_select, {
      # Randomly sample a measure from each category
      random_measure1 <- sample(c(inequality_measures, "homicide_rate", stock_indices), 1)
      random_measure2 <- sample(c(inequality_measures, "homicide_rate", stock_indices), 1)
      

      # Update the UI with random selections
      updateSelectInput(session, "measure1", selected = random_measure1)
      updateSelectInput(session, "measure2", selected = random_measure2)
    })
  
    output$rate_plot <- renderPlotly({
      req(input$measure1, input$measure2, input$country)
      
      measure1 <- input$measure1
      measure2 <- input$measure2
      
      measures_inequ <- c(inequality_measures, homicide_measure)
      measures_stock <- stock_indices
      
      # Generate a unique color for each country
      selected_countries <- input$country
      country_colors <- setNames(RColorBrewer::brewer.pal(min(length(selected_countries), 8), "Set1"), selected_countries)
      
      # Initialize plotly object
      plot <- plot_ly()
      
      # Filter data for inequ_homicide_data
      filtered_inequ_data <- inequ_homicide_data %>%
        filter(country %in% selected_countries) %>%
        rename(Date = year_date)
      
      # Filter data for finance_data_long
      filtered_finance_data <- finance_data_long %>%
        filter(Index %in% c(measure1, measure2))
      
      # Add lines for the first measure
      for (country in selected_countries) {
        if (measure1 %in% measures_inequ) {
          country_data <- filtered_inequ_data %>%
            filter(country == !!country)
          plot <- plot %>%
            add_lines(
              x = country_data$Date,
              y = country_data[[measure1]],
              name = paste(country, "-", measure1),
              yaxis = "y1",
              line = list(color = country_colors[country], dash = "solid")
            )
        } else if (measure1 %in% measures_stock) {
          stock_data <- filtered_finance_data %>%
            filter(Index == measure1)
          plot <- plot %>%
            add_lines(
              x = stock_data$Date,
              y = stock_data$Close,
              name = paste(country, "-", measure1),
              yaxis = "y1",
              line = list(color = country_colors[country], dash = "solid")
            )
        }
      }
      
      # Add lines for the second measure
      for (country in selected_countries) {
        if (measure2 %in% measures_inequ) {
          country_data <- filtered_inequ_data %>%
            filter(country == !!country)
          plot <- plot %>%
            add_lines(
              x = country_data$Date,
              y = country_data[[measure2]],
              name = paste(country, "-", measure2),
              yaxis = "y2",
              line = list(color = country_colors[country], dash = "dash")
            )
        } else if (measure2 %in% measures_stock) {
          stock_data <- filtered_finance_data %>%
            filter(Index == measure2)
          plot <- plot %>%
            add_lines(
              x = stock_data$Date,
              y = stock_data$Close,
              name = paste(country, "-", measure2),
              yaxis = "y2",
              line = list(color = country_colors[country], dash = "dash")
            )
        }
      }
      
      # Layout adjustments
      plot <- plot %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = measure1, side = "left"),
          yaxis2 = list(title = measure2, overlaying = "y", side = "right"),
          legend = list(
            orientation = "h",
            y = -0.1,
            x = 0.5,
            xanchor = "center"
          ),
          margin = list(b = 100, l = 50, r = 50, t = 0)
        )
      
      # Add world events if checkbox is selected
      if (input$show_events) {
        events <- data.frame(
          date = as.Date(c("1991-01-01", "1997-01-01", "2000-01-01", "2008-01-01", "2020-01-01")),
          label = c("Fall of the Soviet Union", "Asian Financial Crisis", "Dotcom Bubble Burst", "Financial Crisis", "COVID-19")
        )
        
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
        
        event_annotations <- lapply(1:nrow(events), function(i) {
          list(
            x = events$date[i],
            y = 0.98,
            xref = "x",
            yref = "paper",
            text = events$label[i],
            showarrow = FALSE,
            xanchor = 'left',
            textangle = -90,
            font = list(color = "grey", size = 12)
          )
        })
        
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
