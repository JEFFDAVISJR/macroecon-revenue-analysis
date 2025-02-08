library(shiny)
library(tidyverse)
library(glue)
library(DT)
library(lmtest)

sales <- read_csv("./data/read_summarized_sales_clean_new.csv")
sales <- sales %>%
  mutate(Sales_Percentile = ntile(`Total Rev`, 100))
gdp <- read_csv("./data/read_gdp_sector_pivot_clean.csv")
gdp_var <- c("Accommodation_Food_Services",	"Agriculture_FFH","Air_Trans","Arts_Entertainment_Rec",	"Construction",	
             "Educational_Health_Social",	"Federal",	"Finance_Ins_RealEstate",	"Information",	"Manufacturing",	
             "Mining", "Other_Services_Not_Government",	"Other_Trans_Support_ Activity",	"Pipeline_Transportation",	
             "Professional_ Business_Services",	"Rail_Transportation",	"Retail_Trade",	"State_Local",	
             "Transit_Ground_Trans","Truck_Trans","Utilities","Warehousing_Storage",	"Water_Trans",
             "Wholesale_Trade",	"GDP_Total"
)
non_gdp <- read_csv("./data/read_n_gdp_var_clean.csv")
non_gdp_var <- c("CCI", "Fed_Funds_Rate", "New_Jobs", "Jet_Fuel", "Unemployment")
month_offset <- c("Year-Month", "Year-Month_Offset1", "Year-Month_Offset2")
qtr_offset <- c("Year-Qtr", "Year-Qtr_Offset1", "Year-Qtr_Offset2")
total_offset_var <-c("Total_Rev", "Total_Rev_Offset1", "Total_Rev_Offset2")
