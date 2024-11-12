library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)



# Import data
homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/homicide-rate-unodc.csv")

head(homicide_data)
inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")

colnames(inequ_data_orig)[3] <- "gini_before_tax"


# 1. Visualization
print(inequ_world)
# Visualization of inequality in the world since 1980 with 4 different measures of inequality
# Subsetting data set 
inequ_world <- inequ_data_orig %>%
  filter(Year >= 1980) %>%
  filter(Country == "World") %>%
  select(1:8, -"Income share of the poorest 50% (before tax) (World Inequality Database)" ) %>%
  rename(
    year ="Year" ,
    gini_coef = "gini_before_tax" ,
    palma_ratio = "Palma ratio (before tax) (World Inequality Database)" ,
    top_10_income_share = "Income share of the richest 10% (before tax) (World Inequality Database)" ,
    top_1_income_share =  "Income share of the richest 1% (before tax) (World Inequality Database)",
    top_01_income_share = "Income share of the richest 0.1% (before tax) (World Inequality Database)")
  

# Scale inequality measures to 0-1
inequ_world <- inequ_world %>%
  mutate(across(4:6, ~ (. / 100)))

  
print(inequ_world)
# Convert data from wide to long format for ggplot2
# Define transformation functions for palma_ratio
max_palma <- max(inequ_world$palma_ratio)
min_palma <- min(inequ_world$palma_ratio)
range_palma <- max_palma - min_palma

# Transformation function (palma_ratio to 0-1 scale)
trans <- function(x) (x - min_palma) / range_palma

# Inverse transformation function (0-1 scale back to palma_ratio)
inv <- function(x) x * range_palma + min_palma
library(reshape2)
# Melt the data for ggplot2
df_melt <- melt(inequ_world, id.vars = c("Country", "year"),
                measure.vars = c("gini_coef", "top_10_income_share", "top_1_income_share", "top_01_income_share"))

# Create the plot
world_inequ_plot <- ggplot() +
  # Plot columns 3 to 6
  geom_line(data = df_melt, aes(x = year, y = value, color = variable)) +
  # Plot palma_ratio on the secondary axis
  geom_line(data = inequ_world, aes(x = year, y = trans(palma_ratio), linetype = "Palma Ratio"), color = "red") +
  # Define y-axes
  scale_y_continuous(
    name = "Values between 0 and 1",
    limits = c(0, 1),
    sec.axis = sec_axis(~ inv(.), name = "Palma Ratio")
  ) +
  # Labels and themes
  labs(x = "Year") +
  scale_color_discrete(name = "Variables") +
  scale_linetype_manual(name = "", values = "dashed") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )



# For G7
inequ_g7 <- inequ_data_orig %>%
  filter(Year >= 1980) %>%
  filter(Country %in% c("United States", "Japan", "Germany", "United Kingdom", "France", "Italy", "Canada")) %>%
  select(1:8) %>%
  rename(
    year = "Year",
    gini_coef = "gini_before_tax",
    palma_ratio = "Palma ratio (before tax) (World Inequality Database)", 
    top_10_income_share = "Income share of the richest 10% (before tax) (World Inequality Database)" ,
    top_1_income_share =  "Income share of the richest 1% (before tax) (World Inequality Database)",
    top_01_income_share = "Income share of the richest 0.1% (before tax) (World Inequality Database)"
  ) %>% 
  na.omit()

print(inequ_g7)

gini_g7 <- ggplot(inequ_g7, aes(x = year, y = gini_coef, color = Country)) +
  geom_line() +
  labs(title = "Gini Coefficient Across Years by Country",
       x = "Year",
       y = "Gini Coefficient") +
  theme_minimal()
gini_g7

# Palmer ratio 
palmer_g7 <- ggplot(inequ_g7, aes(x = year, y = palma_ratio, color = Country)) +
  geom_line() +
  labs(title = "Palma Ratio Across Years by Country",
       x = "Year",
       y = "Palma Ratio") +
  theme_minimal()

palmer_g7

# 0.1 
zero_one_g7 <- ggplot(inequ_g7, aes(x = year, y = top_01_income_share, color = Country)) +
  geom_line() +
  labs(title = "Top 0.1% Income Share Across Years by Country",
       x = "Year",
       y = "Top 0.1% Income Share") +
  theme_minimal()
zero_one_g7

# 10% 
ten_g7 <- ggplot(inequ_g7, aes(x = year, y = top_10_income_share, color = Country)) +
  geom_line() +
  labs(title = "Top 10% Income Share Across Years by Country",
       x = "Year",
       y = "Top 10% Income Share") +
  theme_minimal()
ten_g7


# Adjust each plot to remove legends as before


gini_g7 <- gini_g7 + theme(legend.position = "none")

palmer_g7 <- palmer_g7 + theme(legend.position = "none")

zero_one_g7 <- zero_one_g7 + theme(legend.position = "none")
# Keep the legend in one plot (ten_g7)

# Use cowplot to arrange the plots in a 2x2 grid with aligned elements
combined_plot <- plot_grid(
  gini_g7, palmer_g7, zero_one_g7, ten_g7,
  ncol = 2, align = "hv", 
  labels = c("A", "B", "C", "D")
)

# Display the combined plot

print(combined_plot)



