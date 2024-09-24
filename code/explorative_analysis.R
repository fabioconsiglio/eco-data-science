library(tidyverse)
library(readr)



homicide_data <- read_csv("/Users/fabianmahner/eco-data-science/data/raw/homicide-rate-unodc.csv")


inequ_data <-  read_csv("/Users/fabianmahner/eco-data-science/data/raw/inequality-dataset.csv")

colnames(inequ_data)[3] <- "gini_before_tax"


ggplot(data = inequ_data, aes(x = Year, y = gini_before_tax,  colour = Country)) + geom_line()



