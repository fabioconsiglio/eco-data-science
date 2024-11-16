library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)



# Start with inequality data 

inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/inequality-dataset.csv")

colnames(inequ_data_orig)[3] <- "gini_before_tax"

inequ_data <- inequ_data_orig %>%
  filter(Year >= 1980) %>%
  select(1:8, -"Income share of the poorest 50% (before tax) (World Inequality Database)" ) %>%
  rename(
    country = "Country",
    year ="Year" ,
    "Gini Coefficient" = "gini_before_tax" ,
    "Palma Ratio" = "Palma ratio (before tax) (World Inequality Database)" ,
    "Top 10 % Income Share" = "Income share of the richest 10% (before tax) (World Inequality Database)" ,
    "Top 1 % Income Share" =  "Income share of the richest 1% (before tax) (World Inequality Database)",
    "Top 0.1 % Income Share" = "Income share of the richest 0.1% (before tax) (World Inequality Database)")


homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/homicide-rate-unodc.csv")
colnames(homicide_data)[1] <- "country"
homicide_data <- homicide_data %>% 
  filter(Year >= 1980) %>% 
  select(1,3,4) %>%
  rename("Homicide Rate" = "Homicide rate per 100,000 population - Both sexes - All ages",
         year = "Year")
         
# merging homicide data with inequality data
inequ_homicide_data <- merge(homicide_data, inequ_data, by.x = c("country", "year"), by.y = c("country", "year"), na.rm = TRUE)
inequ_homicide_data <- inequ_homicide_data %>%
  mutate(year_date = as.Date(paste0(year, "-01-01")))

n_distinct(inequ_homicide_data$country)


write.csv(inequ_homicide_data, "/Users/fabianmahner/eco-data-science/ggplot_project/shiny/inequ_homicide_data.csv")

# STOCK MARKET DATA
fin_data <- read_csv("/Users/fabianmahner/eco-data-science/ggplot_project/data/raw/INDICES_DATA.csv")
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
fin_data_clean <- fin_data_clean %>%
  rename(
    Date = Date,
    `S&P 500` = GSPC_Close,
    `Dow Jones` = DJI_Close,
    `DAX` = GDAXI_Close,
    `Nikkei` = N225_Close
  )
fin_data_long <- fin_data_clean %>%
  pivot_longer(cols = -Date, names_to = "Index", values_to = "Close") %>%
  mutate(Close = as.numeric(Close)) # Convert Close values to numeric for plotting


write.csv(fin_data_long, "/Users/fabianmahner/eco-data-science/ggplot_project/shiny/stock_data.csv")


print(fin_data_clean)
