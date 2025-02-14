#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

fluidPage(
  titlePanel(
    HTML('<span style="font-size: 24px; 
         font-weight: bold; 
         color: white; background: 
         linear-gradient(90deg, #2196F3, #4CAF50); 
         padding: 8px 12px; 
         border-radius: 4px;">Economic Indicators & Revenue</span>')
  ),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 # Select an order class from the data or All
                 radioButtons("S_Cons_Order_Class",
                              label = "Select an Order Class", 
                              choices = c("All Sales", sales |> distinct(S_Cons_Order_Class) |> pull(S_Cons_Order_Class) |> sort())),
                 
                 # Slider for Sales Percentile range
                 sliderInput("slider2", 
                             label = "Sales Percentile Range", 
                             min = 0, 
                             max = 100, 
                             value = c(10, 90),  # Set default range (e.g., 10th to 90th percentile)
                             step = 1, 
                             animate = TRUE),
                 
                 # Conditional drop down (Monthly Economic Indicator tab x-axis var/scatter plot)
                 conditionalPanel(
                   condition = "input.tabs == 'Linear Regression (Monthly)'",  # Show on Monthly Economic Indicator tab
                   selectInput("line_x_var", 
                               label = "Select Variable for X-Axis", 
                               choices = c("New_Jobs", "CCI", "Fed_Funds_Rate", "Jet_Fuel", "Unemployment"))
                 ),
                 
                 # Conditional drop down (Quarterly Economic Indicators tab x-axis var/line plot)
                 conditionalPanel(
                   condition = "input.tabs == 'Linear Regression (Quarterly)'",  
                   selectInput("line_x_var_gdp", 
                               label = "Select Variable for X-Axis", 
                               choices = c("Accommodation_Food_Services", "Agriculture_FFH", "Air_Trans", "Arts_Entertainment_Rec", 
                                           "Construction", "Educational_Health_Social", "Federal", "Finance_Ins_RealEstate", 
                                           "Information", "Manufacturing", "Mining", "Other_Services_Not_Government", 
                                           "Other_Trans_Support_Activity", "Pipeline_Transportation", "Professional_Business_Services", 
                                           "Rail_Transportation", "Retail_Trade", "State_Local", "Transit_Ground_Trans", "Truck_Trans", 
                                           "Utilities", "Warehousing_Storage", "Water_Trans", "Wholesale_Trade", "GDP_Total"))
                 ),
          
                 # Conditional drop down (Monthly Arima Model X-reg)
                 conditionalPanel(
                   condition = "input.tabs == 'ARIMA (Monthly)'",  
                   selectInput("monthly_x_reg", 
                               label = "Select Variable", 
                               choices = c("New_Jobs", "CCI", "Fed_Funds_Rate", "Jet_Fuel", "Unemployment"))
                 ),
                 
                 # Conditional drop down (Quarterly Arima Model x-reg var)
                 conditionalPanel(
                   condition = "input.tabs == 'ARIMA (Quarterly)'",  
                   selectInput("scatter_x_reg", 
                               label = "Select Variable", 
                               choices = c("Accommodation_Food_Services", "Agriculture_FFH", "Air_Trans", "Arts_Entertainment_Rec", 
                                           "Construction", "Educational_Health_Social", "Federal", "Finance_Ins_RealEstate", 
                                           "Information", "Manufacturing", "Mining", "Other_Services_Not_Government", 
                                           "Other_Trans_Support_Activity", "Pipeline_Transportation", "Professional_Business_Services", 
                                           "Rail_Transportation", "Retail_Trade", "State_Local", "Transit_Ground_Trans", "Truck_Trans", 
                                           "Utilities", "Warehousing_Storage", "Water_Trans", "Wholesale_Trade", "GDP_Total"))
                   
                 ),
                 
                 # Conditional display (Monthly Economic Indicators linear regression model summary)
                 conditionalPanel(
                   condition = "input.tabs == 'Linear Regression (Monthly)'",  # Only show in Monthly Economic Indicator tab
                   fluidRow(
                     column(width = 12,
                            tags$strong("Linear Regression Summary"),
                            verbatimTextOutput("lmSummary")  # Display the regression summary for Economic Indicator
                     )
                   )
                 ),
                 
                 # Conditional display (Quarterly Economic Indicators linear regression model summary)
                 conditionalPanel(
                   condition = "input.tabs == 'Linear Regression (Quarterly)'",  # Only show in Quarterly Economic Indicator (GDP) tab
                   fluidRow(
                     column(width = 12,
                            tags$strong("Linear Regression Summary"),
                            verbatimTextOutput("lmSummaryGDP")  # Display the regression summary for GDP
                     )
                   )
                 ),
                 
                 # Conditional display (Monthly ARIMA Model sidebar summary)
                 conditionalPanel(
                   condition = "input.tabs == 'ARIMA (Quarterly)'",
                   fluidRow(
                     column(width = 12,
                            tags$strong("ARIMA Model Summary"),
                            verbatimTextOutput("arimaSummary")  # Display the ARIMA model summary in the sidebar
                     )
                   )
                 ),
                 # Conditional display (Monthly ARIMA Model sidebar summary)
                 conditionalPanel(
                   condition = "input.tabs == 'ARIMA (Monthly)'",
                   fluidRow(
                     column(width = 12,
                            tags$strong("ARIMA Model Summary"),
                            verbatimTextOutput("ArimaMonthlySummary")  # Display the ARIMA model summary in the sidebar
                     )
                   )
                 )
    ),
    
    # Show plots in tabs
    mainPanel(
      tabsetPanel(id = "tabs",  # Assign an id to the tabsetPanel for reference in conditionalPanel
                  
                  # Monthly Economic Indicators
                  tabPanel(
                    "Linear Regression (Monthly)", 
                    fluidRow(
                      column(width = 12,
                             div(class = "plot-container", plotOutput("distPlot")))  # dist plot
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "border: 2px solid #007bff; padding: 10px; background-color: #f7f7f7; border-radius: 8px;", 
                               plotOutput("linePlot", height = "300px")
                             ))  #
                    ),
                    
                    # Table below scatter
                    fluidRow(
                      column(width = 12,
                             tags$strong("Underlying Scatter Plot Data"),
                             DT::dataTableOutput("aggregatedDataTable") 
                      )
                    )
                  ),
                  
                  # Quarterly Economic Indicators
                  tabPanel(
                    "Linear Regression (Quarterly)",
                    fluidRow(
                      column(width = 12,
                             div(class = "plot-container", plotOutput("distPlotGDP")))  # Same histogram here
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "border: 2px solid #007bff; padding: 10px; background-color: #f7f7f7; border-radius: 8px;", 
                               plotOutput("linePlotGDP", height = "300px")
                             ))  # scatter plot with border wrapped in div
                    ),
                    
                    # Table below scatter
                    fluidRow(
                      column(width = 12,
                             tags$strong("Underlying Scatter Plot Data"),
                             DT::dataTableOutput("AggregatedDataTableGDP") 
                      )
                    )
                  ),
                  
                  # Forecast Model (Monthly)
                  tabPanel(
                    "ARIMA (Monthly)",
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Monthly Results"),
                             plotOutput("Arima_Monthly_Plot")  # Placeholder for ARIMA plot for monthly data
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Monthly Underlying Data"),
                             DT::dataTableOutput("AggregatedDataTable_Monthly_Arima")  # Table for monthly ARIMA data
                      )
                    )
                    
                  ),
                  
                  # Forecast Model (Quarterly)
                  tabPanel(
                    "ARIMA (Quarterly)",
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Results"),
                             plotOutput("arimaPlot")  # Placeholder for ARIMA plot
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Underlying Data"),
                             DT::dataTableOutput("AggregatedDataTable_GDP_Arima")  # Table for ARIMA data
                      )
                    )
                    
                  )
      )  
    )  
  )  
)




































