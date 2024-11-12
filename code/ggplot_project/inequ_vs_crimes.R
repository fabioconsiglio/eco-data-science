library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)


# Import data
homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/homicide-rate-unodc.csv")

head(homicide_data)
inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")

colnames(inequ_data_orig)[3] <- "gini_before_tax"

print(homicide_data)


homicide_data_2017 <- homicide_data %>%
  filter(Year == 2017) %>%
  select(1, 4) %>%
  rename(
    Country = "Entity",
    Homicide = "Homicide rate per 100,000 population - Both sexes - All ages")

  #filter(Entity %in% c("United States", "Japan", "Germany", "United Kingdom", "France", "Italy", "Canada")) %>%
inequ_data_2017 <- inequ_data_orig %>% 
  filter(Year == 2017) %>%
  select(1:8)

  #filter(Country %in% c("United States", "Japan", "Germany", "United Kingdom", "France", "Italy", "Canada")) %>%

# 1. Merging 
# Merge homicide and inequality data
merged_data <- merge(homicide_data_2017, inequ_data_2017, by.x = "Country", by.y = "Country", na.rm = TRUE)
merged_data <- merged_data[complete.cases(merged_data), ]
# top and down remove outliers



# Visualizatiion 
# Create a plot
inequ_vs_crimes_plot <- ggplot(merged_data, aes(x = gini_before_tax, y = Homicide)) +
  geom_point(aes(color = Country), size = 3) +
  #geom_text(aes(label = Country), hjust = 0, vjust = 0) + maybe display only outliers 
  geom_smooth(method = "lm", se = FALSE, color = "black") + # Add correlation line
  labs(title = "Homicide rate vs. Gini coefficient in 2017",
       x = "Gini coefficient",
       y = "Homicide rate per 100,000 population") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(
    limits = c(0, 15))





inequ_vs_crimes_plot


corr <- cor(merged_data$gini_before_tax, merged_data$Homicide)
corr



# Inequality and crime rate across continents 
continent = c("Africa (WID)", "Asia (WID)", "Europe (WID)", "North America (WID)", "Oceania (WID)", "South America (WID)")



# Filter merged_data where Country contains any of the continent substrings
inequ_crime_continent <- inequ_data_2017 %>%
  filter(Country %in% continent)


