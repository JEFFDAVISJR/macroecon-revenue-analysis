library(shiny)
library(tidyverse)
library(glue)
library(DT)

sales <- read_csv("./data/read_summarized_sales_clean.csv")
sales <- sales %>%
  mutate(Sales_Percentile = ntile(`Total Rev`, 100))

gdp <- read_csv("./data/read_gdp_sector_pivot_clean.csv")
non_gdp <- read_csv("./data/read_n_gdp_var_clean.csv")
non_gdp_var <- c("CCI", "Fed_Fund_Rate", "New_Jobs", "Jet Fuel (Price/Gallon)", "Unemployment")


