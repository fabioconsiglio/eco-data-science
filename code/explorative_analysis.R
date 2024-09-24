library(tidyverse)
library(readr)
library(dplyr)


homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/homicide-rate-unodc.csv")


inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")

colnames(inequ_data_orig)[3] <- "gini_before_tax"


inequ_data <- inequ_data_orig %>%
  filter(Year >= 1974, Country %in% c('France', 'Germany', 'Norway', 'Spain',"Peru", "Argentina", "Chile", "Sub-Saharan Africa (WID)"))




ggplot(data = inequ_data, aes(x = Year, y = gini_before_tax,  colour = Country)) +geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Gini Coefficient Over the Years",
    x = "Year",
    y = "Gini Coefficient"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


