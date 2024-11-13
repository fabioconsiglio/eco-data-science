library(tidyverse)
library(readr)
library(dplyr)
library(cowplot)



# Start with inequality data 

inequ_data_orig <-  read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")

colnames(inequ_data_orig)[3] <- "gini_before_tax"

inequ_data <- inequ_data_orig %>%
  filter(Year >= 1980) %>%
  select(1:8, -"Income share of the poorest 50% (before tax) (World Inequality Database)" ) %>%
  rename(
    country = "Country",
    year ="Year" ,
    gini_coef = "gini_before_tax" ,
    palma_ratio = "Palma ratio (before tax) (World Inequality Database)" ,
    top_10_income_share = "Income share of the richest 10% (before tax) (World Inequality Database)" ,
    top_1_income_share =  "Income share of the richest 1% (before tax) (World Inequality Database)",
    top_01_income_share = "Income share of the richest 0.1% (before tax) (World Inequality Database)")


homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/homicide-rate-unodc.csv")
colnames(homicide_data)[1] <- "country"
homicide_data <- homicide_data %>% 
  filter(Year >= 1980) %>% 
  select(1,3,4) %>%
  rename(homicide_rate = "Homicide rate per 100,000 population - Both sexes - All ages",
         year = "Year")
         

# merging homicide data with inequality data
inequ_homicide_data <- merge(homicide_data, inequ_data, by.x = c("country", "year"), by.y = c("country", "year"), na.rm = TRUE)


write.csv(inequ_homicide_data, "data/inequ_homicide_data.csv")

