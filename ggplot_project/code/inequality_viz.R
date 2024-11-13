library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)



# Import data

inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/code/ggplot_project/data/raw/inequality-dataset.csv")

colnames(inequ_data_orig)[3] <- "gini_before_tax"


# 1. Visualization
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

library(reshape2)
# Melt the data for ggplot2
df_melt <- melt(inequ_world, id.vars = c("Country", "year"),
                measure.vars = c("gini_coef", "top_10_income_share") )#, "top_1_income_share", "top_01_income_share"))

# Create the plot
world_inequ_plot <- ggplot() +
  # Plot gini_coef on the primary y-axis
  geom_line(data = inequ_world, aes(x = year, y = gini_coef, color = "Gini Coefficient")) +
  # Plot top_10_income on the primary y-axis
  geom_line(data = inequ_world, aes(x = year, y = top_10_income_share, color = "Top 10% Income Share")) +
  # Plot palma_ratio on the secondary y-axis
  geom_line(data = inequ_world, aes(x = year, y = palma_ratio, linetype = "Palma Ratio"), color = "red") +
  # Define y-axes
  scale_y_continuous(
    name = "Values for Gini and Top 10% Income",
    limits = c(0.5, 0.75), # Adjust these limits as needed
    sec.axis = sec_axis(~ ., name = "Palma Ratio (10-15 range)")
  ) +
  # Labels and themes
  labs(x = "Year") +
  scale_color_manual(name = "Variables", values = c("Gini Coefficient" = "blue", "Top 10% Income Share" = "green")) +
  scale_linetype_manual(name = "", values = "dashed") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

world_inequ_plot

## palmer ratio is missing 





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
  labs(title = "Gini Coefficient (pre-tax)",
       x = "Year",
       y = "Gini Coefficient") +
  theme_minimal() +
  
#export plot 
pdf(file = 'figures/gini_g7_across_years.pdf', width = 6, 
    height = 4)
gini_g7
dev.off()

# Palmer ratio 
palmer_g7 <- ggplot(inequ_g7, aes(x = year, y = palma_ratio, color = Country)) +
  geom_line() +
  labs(title = "Palma Ratio (pre-tax)",
       x = "Year",
       y = "Palma Ratio") +
  theme_minimal()

#export plot 
pdf(file = 'figures/palmer_g7_across_years.pdf', width = 6, 
    height = 4)
palmer_g7

dev.off()

# 0.1 
zero_one_g7 <- ggplot(inequ_g7, aes(x = year, y = top_01_income_share, color = Country)) +
  geom_line() +
  labs(title = "Top 0.1% Income Share (pre-tax)",
       x = "Year",
       y = "Income Share in %") +
  theme_minimal()
zero_one_g7

# 10% 
ten_g7 <- ggplot(inequ_g7, aes(x = year, y = top_10_income_share, color = Country)) +
  geom_line() +
  labs(title = "Top 10% Income Share (pre-tax)",
       x = "Year",
       y = "Income Share in %") +
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

pdf(file = 'figures/inequality_measures.pdf', width = 16, 
    height = 10)
combined_plot
dev.off()
print(combined_plot)


# Highlight important events in world history 
gini_g7 <- ggplot(inequ_g7, aes(x = year, y = gini_coef, color = Country)) +
  geom_line() +
  labs(
       x = "Year",
       y = "Gini Coefficient") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 15),  # Adjust the size of legend labels
    legend.title = element_text(size = 18),
    axis.title.x = element_text(size = 14),      # Adjust the size of x-axis label
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),       # Adjust the size of x-axis numbers
    axis.text.y = element_text(size = 12) # Adjust the size of legend title
  )+
  geom_vline(xintercept = 1991, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 1997, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey") +

  # Add annotations for events
  annotate("text", x = 1991, y = max(inequ_g7$gini_coef)*0.97, label = "Fall of the Soviet Union", angle = 90, vjust = -0.5, color = "black", size = 5)+
  annotate("text", x = 1997, y = max(inequ_g7$gini_coef)*0.97, label = "Asian Financial Crisis", angle = 90, vjust = -0.5, color = "black", size = 5)+
  annotate("text", x = 2000, y = max(inequ_g7$gini_coef)*0.97, label = "Dotcom Bubble Burst", angle = 90, vjust = -0.5, color = "black", size = 5)+
  annotate("text", x = 2008, y = max(inequ_g7$gini_coef)*0.97, label = "Financial Crisis", angle = 90, vjust = -0.5, color = "black", size = 5) +
  annotate("text", x = 2020, y = max(inequ_g7$gini_coef)*0.99, label = "COVID-19", angle = 90, vjust = -0.5, color = "black", size = 5)

pdf(file = 'figures/gini_g7_incl_world_events.pdf', width = 16, 
    height = 10)
gini_g7

dev.off()


