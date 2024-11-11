library(tidyverse)
library(readr)
library(dplyr)

# Import data
homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/homicide-rate-unodc.csv")

head(homicide_data)
inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")

colnames(inequ_data_orig)[3] <- "gini_before_tax"

# Filter data - 
inequ_data <- inequ_data_orig %>%
  filter(Year >= 1975, Country %in% c("United States", 'France', 'Germany', 'Norway', "Hungary", 'Spain',"Peru", "Argentina", "Chile", "Sub-Saharan Africa (WID)", "China" )) %>%
  select(1:8)

# World
# Across Continents

# Various democracies vs. autoritarian regimes




ggplot(data = inequ_data, aes(x = Year, y = gini_before_tax,  colour = Country)) +geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Gini Coefficient Over the Years",
    x = "Year",
    y = "Gini Coefficient" 
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


