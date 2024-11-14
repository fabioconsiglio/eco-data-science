library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)


# Import data
homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/homicide-rate-unodc.csv")

head(homicide_data)

inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/inequality-dataset.csv")


print(homicide_data)

homicide_data_2017 <- homicide_data %>%
  filter(Year == 2017) %>%
  select(1, 4) %>%
  rename(
    Country = "Entity",
    Homicide = "Homicide rate per 100,000 population - Both sexes - All ages")

inequ_data_2017 <- inequ_data_orig %>% 
  filter(Year == 2017) %>%
  select(1:8) %>%
  rename (
    gini_coef = "Gini coefficient (before tax) (World Inequality Database)"
  )


# 1. Merging 
# Merge homicide and inequality data
merged_data <- merge(homicide_data_2017, inequ_data_2017, by.x = "Country", by.y = "Country", na.rm = TRUE)
merged_data <- merged_data[complete.cases(merged_data), ]

# top and down remove outliers
merged_data <- merged_data %>% 
  filter(Homicide < 12.5)


# Visualizatiion 
# Create a plot
inequ_vs_crimes_plot <- ggplot(merged_data, aes(x = gini_coef, y = Homicide)) +
  geom_point(aes(color = Country), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 0.5) + # Add correlation line
  labs(
       x = "Gini coefficient",
       y = "Homicide rate (per 100,000 population)") +
  theme_minimal() +
  theme(legend.position = "none",
          axis.title.x = element_text(size = 14), # Adjust the size of x-axis label
          axis.title.y = element_text(size = 14), # Adjust the size of y-axis label
          axis.text.x = element_text(size = 12),  # Adjust the size of x-axis numbers
          axis.text.y = element_text(size = 12)   # Adjust the size of legend title
        ) +
  scale_y_continuous(
    limits = c(0, 12.5))                          # Adjust the y-axis limits


pdf(file = 'figures/gini_vs_crimes.pdf', width = 16, 
    height = 10)
inequ_vs_crimes_plot

dev.off()


inequ_vs_crimes_plot


corr <- cor(merged_data$gini_coef, merged_data$Homicide)
corr




