library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)
library(reshape2)



# Importing the data

inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/inequality-dataset.csv")

# Get column names for renaming 
print(inequ_data_orig)

# Subsetting Dataset for G7 - keeping only from 1980 onwards. 
inequ_g7 <- inequ_data_orig %>%
  filter(Year >= 1980) %>% # Filter for years after 1980
  filter(Country %in% c("United States", "Japan", "Germany", "United Kingdom", "France", "Italy", "Canada")) %>% # Filter for G7
  select(1:8) %>% # Select only relevant measures 
  rename(
    year = "Year",
    gini_coef = "Gini coefficient (before tax) (World Inequality Database)",
    palma_ratio = "Palma ratio (before tax) (World Inequality Database)", 
    top_10_income_share = "Income share of the richest 10% (before tax) (World Inequality Database)" ,
    top_1_income_share =  "Income share of the richest 1% (before tax) (World Inequality Database)",
    top_01_income_share = "Income share of the richest 0.1% (before tax) (World Inequality Database)"
  ) %>% 
  na.omit()

print(inequ_g7)


# Visualizations 

# Palmer ratio (top 10% income share divided by bottom 40% income share)

palmer_g7 <- ggplot(inequ_g7, aes(x = year, y = palma_ratio, color = Country)) +
  geom_line() +
  labs(title = "Palma Ratio (pre-tax)",
       x = "Year",
       y = "Palma Ratio") +
  theme_minimal()
palmer_g7

# Top 1% income share

top_1_g7 <- ggplot(inequ_g7, aes(x = year, y = top_1_income_share, color = Country)) +
  geom_line() +
  labs(title = "Top 1% Income Share (pre-tax)",
       x = "Year",
       y = "Income Share (in %)") +
  theme_minimal()

top_1_g7


# 0.1 % income share
zero_one_g7 <- ggplot(inequ_g7, aes(x = year, y = top_01_income_share, color = Country)) +
  geom_line() +
  labs(title = "Top 0.1% Income Share (pre-tax)",
       x = "Year",
       y = "Income Share (in %)") +
  theme_minimal()
zero_one_g7

# 10% income share 
ten_g7 <- ggplot(inequ_g7, aes(x = year, y = top_10_income_share, color = Country)) +
  geom_line() +
  labs(title = "Top 10% Income Share (pre-tax)",
       x = "Year",
       y = "Income Share (in %)") +
  theme_minimal()
ten_g7


# Remove legends for three plots to reduce redundancy

palmer_g7 <- palmer_g7 + theme(legend.position = "none")
top_1_g7 <- top_1_g7 + theme(legend.position = "none")
zero_one_g7 <- zero_one_g7 + theme(legend.position = "none")

# Keep the legend in one plot (ten_g7)

# Use cowplot to arrange the plots in a 2x2 grid
combined_plot <- plot_grid(
  palmer_g7, zero_one_g7, top_1_g7, ten_g7,
  ncol = 2, align = "hv", 
  labels = c("A", "B", "C", "D")
)

# Display the combined plot and export the plot

pdf(file = 'figures/inequality_measures.pdf', width = 16, 
    height = 10)
combined_plot
dev.off()
print(combined_plot)


# Highlight important events in world history and display the Gini coefficient for G7 countries
gini_g7 <- ggplot(inequ_g7, aes(x = year, y = gini_coef, color = Country)) +
  geom_line(size = 1) + # Increase the line size 
  labs(
       x = "Year",
       y = "Gini Coefficient") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 15),  # Adjust the size of legend labels
    legend.title = element_text(size = 18), # Adjust the size of legend title
    axis.title.x = element_text(size = 14), # Adjust the size of x-axis label
    axis.title.y = element_text(size = 14), # Adjust the size of y-axis label
    axis.text.x = element_text(size = 12),  # Adjust the size of x-axis numbers
    axis.text.y = element_text(size = 12)   # Adjust the size of y-axis numbers
  )+
  geom_vline(xintercept = 1991, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 1997, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey") +

  # Adding annotations for events
  annotate("text", x = 1991, y = max(inequ_g7$gini_coef)*0.97, label = "Fall of the Soviet Union", angle = 90, vjust = -0.5, color = "black", size = 5)+
  annotate("text", x = 1997, y = max(inequ_g7$gini_coef)*0.97, label = "Asian Financial Crisis", angle = 90, vjust = -0.5, color = "black", size = 5)+
  annotate("text", x = 2000, y = max(inequ_g7$gini_coef)*0.97, label = "Dotcom Bubble Burst", angle = 90, vjust = -0.5, color = "black", size = 5)+
  annotate("text", x = 2008, y = max(inequ_g7$gini_coef)*0.97, label = "Financial Crisis", angle = 90, vjust = -0.5, color = "black", size = 5) +
  annotate("text", x = 2020, y = max(inequ_g7$gini_coef)*0.99, label = "COVID-19", angle = 90, vjust = -0.5, color = "black", size = 5)+
  guides(color = guide_legend(override.aes = list(size = 20)))

# Export the plot
pdf(file = 'figures/gini_g7_incl_world_events.pdf', width = 16, 
    height = 10)
gini_g7

dev.off()


