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
                 
                 # Conditional dropdown for x-axis var in scatter plot for Economic Indicator Comparison tab
                 conditionalPanel(
                   condition = "input.tabs == 'Economic Indicator Comparison'",  # Show on Economic Indicator tab
                   selectInput("scatter_x_var", 
                               label = "Select Variable for X-Axis", 
                               choices = c("New_Jobs", "CCI", "Fed_Funds_Rate", "Jet_Fuel", "Unemployment"))
                 ),
                 
                 # Conditional dropdown for Y-axis variable in scatter plot for Economic Indicator Comparison tab
                 conditionalPanel(
                   condition = "input.tabs == 'Economic Indicator Comparison'",  # Show on Economic Indicator tab
                   selectInput("scatter_y_var", 
                               label = "Select Variable for Y-Axis", 
                               choices = c("Total_Rev", "Total_Rev_Offset1", "Total_Rev_Offset2"))
                 ),
                 
                 # Conditional dropdown for x-axis var in scatter plot for GDP Sector Comparison tab
                 conditionalPanel(
                   condition = "input.tabs == 'GDP Sector Comparison'",  # Show on GDP tab
                   selectInput("scatter_x_var_gdp", 
                               label = "Select Variable for X-Axis (GDP)", 
                               choices = c("Accommodation_Food_Services", "Agriculture_FFH", "Air_Trans", "Arts_Entertainment_Rec", 
                                           "Construction", "Educational_Health_Social", "Federal", "Finance_Ins_RealEstate", 
                                           "Information", "Manufacturing", "Mining", "Other_Services_Not_Government", 
                                           "Other_Trans_Support_Activity", "Pipeline_Transportation", "Professional_Business_Services", 
                                           "Rail_Transportation", "Retail_Trade", "State_Local", "Transit_Ground_Trans", "Truck_Trans", 
                                           "Utilities", "Warehousing_Storage", "Water_Trans", "Wholesale_Trade", "GDP_Total"))
                 ),
                 
                 # Conditional dropdown for x-axis var in scatter plot for Arima tab
                 conditionalPanel(
                   condition = "input.tabs == 'Arima Model'",  # Show on Arima tab
                   selectInput("scatter_x_reg", 
                               label = "Select Variable for X-Axis (GDP)", 
                               choices = c("Accommodation_Food_Services", "Agriculture_FFH", "Air_Trans", "Arts_Entertainment_Rec", 
                                           "Construction", "Educational_Health_Social", "Federal", "Finance_Ins_RealEstate", 
                                           "Information", "Manufacturing", "Mining", "Other_Services_Not_Government", 
                                           "Other_Trans_Support_Activity", "Pipeline_Transportation", "Professional_Business_Services", 
                                           "Rail_Transportation", "Retail_Trade", "State_Local", "Transit_Ground_Trans", "Truck_Trans", 
                                           "Utilities", "Warehousing_Storage", "Water_Trans", "Wholesale_Trade", "GDP_Total"))
                 ),
                 
                 # Conditional dropdown for Y-axis variable in scatter plot for GDP Sector Comparison tab
                 conditionalPanel(
                   condition = "input.tabs == 'GDP Sector Comparison'",  # Show on GDP tab
                   selectInput("scatter_y_var_gdp", 
                               label = "Select Variable for Y-Axis (GDP)", 
                               choices = c("Total_Rev", "Total_Rev_Offset1", "Total_Rev_Offset2"))
                 ),
                 
                 # puy lm summ underneath the filters for Economic Indicator tab
                 conditionalPanel(
                   condition = "input.tabs == 'Economic Indicator Comparison'",  # Only show in Economic Indicator Comparison tab
                   fluidRow(
                     column(width = 12,
                            tags$strong("Linear Regression Summary"),
                            verbatimTextOutput("lmSummary")  # Display the regression summary for Economic Indicator
                     )
                   )
                 ),
                 
                 # gdp output summary in sidepanel
                 conditionalPanel(
                   condition = "input.tabs == 'GDP Sector Comparison'",  # Only show in GDP Sector Comparison tab
                   fluidRow(
                     column(width = 12,
                            tags$strong("Linear Regression Summary for GDP"),
                            verbatimTextOutput("lmSummaryGDP")  # Display the regression summary for GDP
                     )
                   )
                 ),
                 
                 # Add more controls here if needed for "Arima Model" tab
    ),
    
    # Show plots in tabs
    mainPanel(
      tabsetPanel(id = "tabs",  # Assign an id to the tabsetPanel for reference in conditionalPanel
                  
                  # First tab: both dist and scatter for Economic Indicators
                  tabPanel(
                    "Economic Indicator Comparison", 
                    fluidRow(
                      column(width = 12,
                             div(class = "plot-container", plotOutput("distPlot")))  # dist plot
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "border: 2px solid #007bff; padding: 10px; background-color: #f7f7f7; border-radius: 8px;", 
                               plotOutput("scatterPlot", height = "300px")
                             ))  # scatter plot with border wrapped in div
                    ),
                    
                    # Table below scatter
                    fluidRow(
                      column(width = 12,
                             tags$strong("Underlying Scatter Plot Data"),
                             DT::dataTableOutput("aggregatedDataTable") 
                      )
                    )
                  ),
                  
                  # GDP Sector Comparison tab
                  tabPanel(
                    "GDP Sector Comparison",
                    fluidRow(
                      column(width = 12,
                             div(class = "plot-container", plotOutput("distPlotGDP")))  # Same histogram here
                    ),
                    fluidRow(
                      column(width = 12,
                             div(
                               style = "border: 2px solid #007bff; padding: 10px; background-color: #f7f7f7; border-radius: 8px;", 
                               plotOutput("scatterPlotGDP", height = "300px")
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
                  
                  # Arima Model tab
                  tabPanel(
                    "Arima Model",
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Results"),
                             plotOutput("arimaPlot")  # Placeholder for ARIMA plot
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Summary"),
                             verbatimTextOutput("arimaSummary")  # Placeholder for ARIMA summary output
                      )
                    )
                  ),
                  
                  # Second tab: 
                  tabPanel(
                    "Facet Grid",
                    tags$strong("Order Class YoY Trends"),
                    fluidRow(
                      column(width = 12,
                             div(class = "plot-container", plotOutput("linePlot")))
                    )
                  )
      )
    )
  )
)
























