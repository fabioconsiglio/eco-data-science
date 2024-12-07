library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)
library(ggplot2)
library(lubridate)


# Import data
fin_data <- read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/INDICES_DATA.csv")
print(fin_data)

# prepare fin data 
fin_data_clean <- fin_data %>%
  select(
    Date = `...1`,       # 
    GSPC_Close = `...5`, # Adjust for the actual position of ^GSPC aka S&P 500
    DJI_Close = `...19`, # Adjust for the actual position of ^DJI aka Dow Jones
    GDAXI_Close = `...61`, # Adjust for the actual position of ^GDAXI aka DAX
    N225_Close = `...103`   # Adjust for the actual position of ^N225 aka Nikkei
  )

# Drop First row
fin_data_clean <- fin_data_clean[-1, ]

# convert date format to date
fin_data_clean$Date <- as.Date(fin_data_clean$Date, format = "%m/%d/%Y")
#rename columns

fin_data_clean <- fin_data_clean %>%
  rename(
    Date = Date,
    `S&P 500` = GSPC_Close,
    `Dow Jones` = DJI_Close,
    `DAX` = GDAXI_Close,
    `Nikkei` = N225_Close
  )

# Converting to long format for plotting
clean_data_long <- fin_data_clean %>%
  pivot_longer(cols = -Date, names_to = "Index", values_to = "Close") %>%
  mutate(Close = as.numeric(Close)) # Convert Close values to numeric for plotting

# Plotting
stock_dev_incl_world_events <- ggplot(clean_data_long, aes(x = Date, y = Close, color = Index)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Close Price",
    color = "Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25), # Adjust the size of the plot title
    legend.key.size = unit(1, 'cm'),        # Change legend key size
    legend.key.height = unit(1, 'cm'),      # Change legend key height
    legend.key.width = unit(1, 'cm'),       # Change legend key width
    legend.position = "bottom",             # Change the position of the legend
    legend.text = element_text(size = 15),  # Adjust the size of legend labels
    legend.title = element_text(size = 18), # Adjust the size of legend title
    axis.title.x = element_text(size = 14), # Adjust the size of x-axis label
    axis.title.y = element_text(size = 14), # Adjust the size of y-axis label
    axis.text.x = element_text(size = 12),  # Adjust the size of x-axis numbers
    axis.text.y = element_text(size = 12),  # Adjust the size of y-axis numbers
  ) +
  # Adding vertical lines for events
  geom_vline(xintercept = as.Date("1991-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("1997-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("2000-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("2008-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", color = "grey") +
  
  # Adding annotations for events
  annotate("text", x = as.Date("1997-01-01"), y = 35000, 
           label = "Asian Financial Crisis", angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = as.Date("2000-01-01"), y = 35000 , 
           label = "Dotcom Bubble Burst", angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = as.Date("2008-01-01"), y = 35000, 
           label = "Financial Crisis", angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = as.Date("2020-01-01"), y = 35000, 
           label = "COVID-19", angle = 90, vjust = -0.5, color = "black")

pdf(file = 'figures/stock_dev_incl_world_events.pdf', width = 16, 
    height = 10)
stock_dev_incl_world_events

dev.off()
stock_dev_incl_world_events 

# top 1% share development as inequality measure and comparing it in four different plots 
# with the corresponding nation for each plot

inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/inequality-dataset.csv")


inequ_g7 <- inequ_data_orig %>%
  filter(Year >= 1980) %>%
  filter(Country %in% c("United States", "Japan", "Germany", "United Kingdom", "Canada")) %>%
  select(1:8) %>%
  rename(
    year = "Year",
    gini_coef = "Gini coefficient (before tax) (World Inequality Database)",
    top_10_income_share = "Income share of the richest 10% (before tax) (World Inequality Database)" ,
    top_1_income_share =  "Income share of the richest 1% (before tax) (World Inequality Database)",
    top_01_income_share = "Income share of the richest 0.1% (before tax) (World Inequality Database)",
    palma_ratio = "Palma ratio (before tax) (World Inequality Database)"
  ) %>% 
  na.omit()




# US development finance and inequality 

inequ_g7


# Convert necessary columns to numeric
# Convert Dow Jones to numeric and extract the year
dow_jones <- fin_data_clean %>%
  mutate(
    `Dow Jones` = as.numeric(`Dow Jones`),
    Date = as.Date(Date),
    year = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(avg_dow_jones = mean(`Dow Jones`, na.rm = TRUE))  # Average Dow Jones by year


## S & P 500
sp500 <- fin_data_clean %>%
  mutate(
    `S&P 500` = as.numeric(`S&P 500`),
    Date = as.Date(Date),
    year = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(avg_sp500 = mean(`S&P 500`, na.rm = TRUE))


# Filter inequality data for Canada and USA
inequality_data_filtered <- inequ_g7 %>%
  filter(Country %in% c("Canada", "United States"))

# Merge data on year for both Canada and USA
merged_data <- inequality_data_filtered %>%
  full_join(dow_jones, by = "year")

merged_data <- merged_data %>%
  full_join(sp500, by = "year")
# keep only relevant columns
merged_data = merged_data[, c("Country","year", "gini_coef", "avg_dow_jones", "palma_ratio", "top_1_income_share")]



# Plotting
dow_jones_inequ <- ggplot(merged_data, aes(x = year)) +
  geom_line(data = subset(merged_data, Country == "Canada"), 
            aes(y = top_1_income_share, color = "Canada"), size = 1) +
  geom_line(data = subset(merged_data, Country == "United States"), 
            aes(y = top_1_income_share, color = "US"), size = 1) +
  geom_line(data = subset(merged_data, Country == "United States"), 
            aes(y = avg_dow_jones / 3000, color = "Dow Jones"), size = 1, linetype = "dashed") +
  scale_x_continuous(
    name = "Year",
    limits = c(1997, max(merged_data$year))) + 
  scale_y_continuous(
    name = "Income Share (in %)",
    sec.axis = sec_axis(~.*3000, name = "Dow Jones Index")
  ) +
  labs(title = "Top 1 % Income Share in US and Canada with Trend of Dow Jones", x = "Year") +
  scale_color_manual(name = "Legend", 
                     values = c("Canada" = "red", "US" = "darkblue", 
                                "Dow Jones" = "skyblue")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "skyblue"),
    legend.position = "bottom"
  )

dow_jones_inequ


# Germany development DAX 
dax <- fin_data_clean %>%
  mutate(
    `DAX` = as.numeric(`DAX`),
    Date = as.Date(Date),
    year = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(avg_dax = mean(`DAX`, na.rm = TRUE))

# Filter inequality data for Germany 
inequality_data_ger<- inequ_g7 %>%
  filter(Country %in% c("Germany"))

# Merge data on year for both Germany
merged_data_ger <- inequality_data_ger %>%
  full_join(dax, by = "year")

# Keep only relevant columns

merged_data_ger = merged_data_ger[, c("Country","year", "top_1_income_share", "avg_dax")]


# Plotting
dax_ineq <- ggplot(merged_data_ger, aes(x = year)) +
  geom_line(aes(y = top_1_income_share, color = "top_1_income_share"), size = 1) +
  geom_line(aes(y = avg_dax / 1000, color = "DAX"), size = 1, linetype = "dashed") + # Scaled to match the other y-axis
  scale_x_continuous(
    name = "Year",
    limits = c(1997, max(merged_data_ger$year))) +
  scale_y_continuous(
    name = "Income Share (in %)",
    sec.axis = sec_axis(~.*1000, name = "DAX Index")
  ) +
  labs(title = "Top 1 % Income Share in Germany with Trend of DAX", x = "Year") +
  scale_color_manual(name = "Legend", values = c("top_1_income_share" = "deeppink", "DAX" = "yellow3"))+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "yellow3"),
    legend.position = "bottom"
  )
dax_ineq

# Japan development Nikkei
nikkei <- fin_data_clean %>%
  mutate(
    `Nikkei` = as.numeric(`Nikkei`),
    Date = as.Date(Date),
    year = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(avg_nikkei = mean(`Nikkei`, na.rm = TRUE))

# filter inequality data  and Japan
inequality_data_jap<- inequ_g7 %>%
  filter(Country %in% c("Japan"))
merged_data_jap <- inequality_data_jap %>%
  full_join(nikkei, by = "year")

# keep only relevant columns

merged_data_jap = merged_data_jap[, c("Country","year", "top_1_income_share", "avg_nikkei")]
# plotting
nikk_ineq <- ggplot(merged_data_jap, aes(x = year)) +
  geom_line(aes(y = top_1_income_share, color = "top_1_income_share"), size = 1) +
  geom_line(aes(y = avg_nikkei / 1000, color = "Nikkei"), size = 1,  linetype = "dashed") +
  scale_x_continuous(
    name = "Year",
    limits = c(1997, max(merged_data_jap$year)))+ # Scaled to match the other y-axis
  scale_y_continuous(
    name = "Income Share (in %)",
    sec.axis = sec_axis(~.*1000, name = "Nikkei Index")
  ) +
  labs(title = "Top 1 % Income Share in Japan with Trend of NIKKEI", x = "Year") +
  scale_color_manual(name = "Legend", values = c("top_1_income_share" = "darkorange", "Nikkei" = "darkgreen"))+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "darkgreen"),
    legend.position = "bottom"
  )

nikk_ineq
# combining all plots 

dow_jones_inequ <- dow_jones_inequ + theme(legend.position = "none")

dax_ineq <- dax_ineq + theme(legend.position = "none")

nikk_ineq <-  nikk_ineq + theme(legend.position = "none")
#### DUMMY LEGEND


# Manually define colors and labels for the legend
legend_labels <- c("Dow Jones", "DAX", "Nikkei", "Canada", "US", "Germany", "Japan")
legend_colors <- c("Dow Jones" = "skyblue", "DAX" = "yellow3", "Nikkei" = "darkgreen", "US" = "darkblue", "Canada" = "red", "Germany" = "deeppink", "Japan" = "darkorange")

# Create a separate plot for the legend
manual_legend <- ggplot(data.frame(x = 1, y = 1, color = legend_labels), aes(x, y, color = color)) +
  geom_line(size = 4) +
  scale_color_manual(values = legend_colors, name = "Indices") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 18)
  )

# Extract just the legend from manual_legend
legend_plot <- get_legend(manual_legend)



# Use cowplot to arrange the plots in a 2x2 grid with aligned elements
combined_plot <- plot_grid(
 dow_jones_inequ, dax_ineq, nikk_ineq, legend_plot,
  ncol = 2, align = "hv", 
  labels = c("A", "B", "C")
)
combined_plot
# Display the combined plot

pdf(file = 'figures/stock_indices_inequ.pdf', width = 16, 
    height = 10)
combined_plot
dev.off()
print(combined_plot)





