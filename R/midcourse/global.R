library(shiny)
library(tidyverse)
library(glue)
library(DT)

sales <- read_csv("./data/read_summarized_sales_clean.csv")
sales <- sales %>%
  mutate(Sales_Percentile = ntile(`Total Rev`, 100))
