library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)


# Import data
fin_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/INDICES_DATA.csv")

print(fin_data)

# prepare fin data 
fin_data_clean <- fin_data %>%
  select(
    Date = `...1`,       # Assuming the date column is the first column
    GSPC_Close = `...5`, # Replace `...6` with the actual position of the ^GSPC close column
    DJI_Close = `...19`, # Replace `...15` with the actual position of the ^DJI close column
    GDAXI_Close = `...61`, # Adjust for the actual position of ^GDAXI
    N225_Close = `...103`   # Adjust for the actual position of ^N225
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

clean_data_long <- fin_data_clean %>%
  pivot_longer(cols = -Date, names_to = "Index", values_to = "Close") %>%
  mutate(Close = as.numeric(Close)) # Convert Close values to numeric for plotting

# Plotting
ggplot(clean_data_long, aes(x = Date, y = Close, color = Index)) +
  geom_line() +
  labs(
    title = "Closing Prices of Major Indices Over Time",
    x = "Date",
    y = "Close Price",
    color = "Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25),
    legend.position = "bottom"
  ) +
  # Add vertical lines for events
  geom_vline(xintercept = as.Date("1991-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("1997-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("2000-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("2008-01-01"), linetype = "dashed", color = "grey") +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", color = "grey") +
  
  # Add annotations for events
  annotate("text", x = as.Date("1997-01-01"), y = 35000, 
           label = "Asian Financial Crisis", angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = as.Date("2000-01-01"), y = 35000 , 
           label = "Dotcom Bubble Burst", angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = as.Date("2008-01-01"), y = 35000, 
           label = "Financial Crisis", angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = as.Date("2020-01-01"), y = 35000, 
           label = "COVID-19", angle = 90, vjust = -0.5, color = "black")

# top 1% share development as inequality measure and comparing it in four different plots 
# with the corresponding nation for each plot
inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")
colnames(inequ_data_orig)[3] <- "gini_before_tax"


inequ_g7 <- inequ_data_orig %>%
  filter(Year >= 1980) %>%
  filter(Country %in% c("United States", "Japan", "Germany", "United Kingdom", "Canada")) %>%
  select(1:8) %>%
  rename(
    year = "Year",
    gini_coef = "gini_before_tax",
    top_10_income_share = "Income share of the richest 10% (before tax) (World Inequality Database)" ,
    top_1_income_share =  "Income share of the richest 1% (before tax) (World Inequality Database)",
    top_01_income_share = "Income share of the richest 0.1% (before tax) (World Inequality Database)"
  ) %>% 
  na.omit()


# US development finance and inequality 

inequ_g7


library(dplyr)
library(ggplot2)
library(lubridate)

# Convert necessary columns to numeric
# Convert Dow Jones to numeric and extract the year
stock_data <- fin_data_clean %>%
  mutate(
    `Dow Jones` = as.numeric(`Dow Jones`),
    Date = as.Date(Date),
    year = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(avg_dow_jones = mean(`Dow Jones`, na.rm = TRUE))  # Average Dow Jones by year

# Filter inequality data for Canada and USA
inequality_data_filtered <- inequ_g7 %>%
  filter(Country %in% c("Canada", "United States"))

# Merge data on year for both Canada and USA
merged_data <- inequality_data_filtered %>%
  full_join(stock_data, by = "year")

# keep only relevant columns
merged_data = merged_data[, c("Country","year", "top_1_income_share", "avg_dow_jones")]

# Plotting

dow_jones_inequ <- ggplot(merged_data, aes(x = year)) +
  geom_line(data = subset(merged_data, Country == "Canada"), 
            aes(y = top_1_income_share, color = "Canada Gini"), size = 1) +
  geom_line(data = subset(merged_data, Country == "United States"), 
            aes(y = top_1_income_share, color = "US Gini"), size = 1) +
  geom_line(data = subset(merged_data, Country == "United States"), 
            aes(y = avg_dow_jones / 3000, color = "US Dow Jones"), size = 1) +
  scale_y_continuous(
    name = "Gini Coefficient",
    sec.axis = sec_axis(~.*3000, name = "Dow Jones Index")
  ) +
  labs(title = "Top 1% income share and Dow Jones Over Time (Canada and US)", x = "Year") +
  scale_color_manual(name = "Legend", 
                     values = c("Canada Top 1%" = "blue", "US Top 1%" = "darkblue", 
                                "Canada Dow Jones" = "red", "US Dow Jones" = "darkred")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "bottom"
  )

# Germany development DAX 
dax <- fin_data_clean %>%
  mutate(
    `DAX` = as.numeric(`DAX`),
    Date = as.Date(Date),
    year = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(avg_dax = mean(`DAX`, na.rm = TRUE))

# filter inequality data for Germany and Japan
inequality_data_ger<- inequ_g7 %>%
  filter(Country %in% c("Germany"))

# Merge data on year for both Germany and Japan
merged_data_ger <- inequality_data_ger %>%
  full_join(dax, by = "year")

# keep only relevant columns

merged_data_ger = merged_data_ger[, c("Country","year", "top_1_income_share", "avg_dax")]


# Plotting
dax_ineq <- ggplot(merged_data_ger, aes(x = year)) +
  geom_line(aes(y = top_1_income_share, color = "top_1_income_share"), size = 1) +
  geom_line(aes(y = avg_dax / 1000, color = "DAX"), size = 1) + # Scaled to match the other y-axis
  #geom_line(aes(y = avg_nikkei / 1000, color = "Nikkei"), size = 1) + # Scaled to match the other y-axis
  scale_y_continuous(
    name = "Top 1 Income Share%",
    sec.axis = sec_axis(~.*1000, name = "DAXIndex")
  ) +
  labs(title = "Top 1% Income share and DAX Over Time", x = "Year") +
  scale_color_manual(name = "Legend", values = c("top_1_income_share" = "blue", "DAX" = "red"))+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
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
inequ_nikk <- ggplot(merged_data_jap, aes(x = year)) +
  geom_line(aes(y = top_1_income_share, color = "top_1_income_share"), size = 1) +
  geom_line(aes(y = avg_nikkei / 1000, color = "Nikkei"), size = 1) + # Scaled to match the other y-axis
  scale_y_continuous(
    name = "Top 1 % Income Share",
    sec.axis = sec_axis(~.*1000, name = "Nikkei Index")
  ) +
  labs(title = "Top 1 % Income Share and Nikkei Over Time", x = "Year") +
  scale_color_manual(name = "Legend", values = c("top_1_income_share" = "blue", "Nikkei" = "green"))+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "green"),
    legend.position = "bottom"
  )





