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
                 
                 # Conditional dropdown for x-axis var in scatter plot for Monthly Economic Indicator Comparison tab
                 conditionalPanel(
                   condition = "input.tabs == 'Monthly Economic Indicators'",  # Show on Monthly Economic Indicator tab
                   selectInput("scatter_x_var", 
                               label = "Select Variable for X-Axis", 
                               choices = c("New_Jobs", "CCI", "Fed_Funds_Rate", "Jet_Fuel", "Unemployment"))
                 ),
                 
                 # Conditional dropdown for Y-axis variable in scatter plot for Monthly Economic Indicator Comparison tab
                 conditionalPanel(
                   condition = "input.tabs == 'Monthly Economic Indicators'",  # Show on Monthly Economic Indicator tab
                   selectInput("scatter_y_var", 
                               label = "Select Variable for Y-Axis", 
                               choices = c("Total_Rev", "Total_Rev_Offset1", "Total_Rev_Offset2"))
                 ),
                 
                 # Add dropdown for X-regression in the "Arima Model Monthly" tab
                 conditionalPanel(
                   condition = "input.tabs == 'Arima Model Monthly'",  # Show on Arima Model Monthly tab
                   selectInput("monthly_x_reg", 
                               label = "Select Variable for X-Axis (ARIMA Regression)", 
                               choices = c("CCI","Fed_Funds_Rate","New_Jobs",	"Jet_Fuel","Unemployment"))
                 ),
                 
                 # put lm summ underneath the filters for Monthly Economic Indicator tab
                 conditionalPanel(
                   condition = "input.tabs == 'Monthly Economic Indicators'",  # Only show in Monthly Economic Indicator Comparison tab
                   fluidRow(
                     column(width = 12,
                            tags$strong("Linear Regression Summary"),
                            verbatimTextOutput("lmSummary")  # Display the regression summary for Economic Indicator
                     )
                   )
                 ),
                 
                 # gdp output summary in sidepanel
                 conditionalPanel(
                   condition = "input.tabs == 'Quarterly Economic Indicators'",  # Only show in Quarterly Economic Indicator (GDP) tab
                   fluidRow(
                     column(width = 12,
                            tags$strong("Linear Regression Summary for GDP"),
                            verbatimTextOutput("lmSummaryGDP")  # Display the regression summary for GDP
                     )
                   )
                 ),
                 
                 # ARIMA Summary output in sidebar
                 conditionalPanel(
                   condition = "input.tabs == 'Arima Model'",  # Only show in ARIMA tab
                   fluidRow(
                     column(width = 12,
                            tags$strong("ARIMA Model Summary"),
                            verbatimTextOutput("arimaSummary")  # Display the ARIMA model summary in the sidebar
                     )
                   )
                 ),
                 
                 # Add more controls later "Arima Model" tab
    ),
    
    # Show plots in tabs
    mainPanel(
      tabsetPanel(id = "tabs",  # Assign an id to the tabsetPanel for reference in conditionalPanel
                  
                  # First tab: both dist and scatter for Monthly Economic Indicators
                  tabPanel(
                    "Monthly Economic Indicators", 
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
                  
                  # Quarterly Economic Indicators tab (GDP Sector Comparison)
                  tabPanel(
                    "Quarterly Economic Indicators",
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
                             tags$strong("ARIMA Model Underlying Data"),
                             DT::dataTableOutput("AggregatedDataTable_GDP_Arima")  # Table for ARIMA data
                      )
                    ),
                    
                  ),
                  
                  # New Arima Model Monthly tab
                  tabPanel(
                    "Arima Model Monthly",
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Monthly Results"),
                             plotOutput("arimamonthlyPlot")  # Placeholder for ARIMA plot for monthly data
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             tags$strong("ARIMA Model Monthly Underlying Data"),
                             DT::dataTableOutput("AggregatedDataTable_Monthly_Arima")  # Table for monthly ARIMA data
                      )
                    ),
                    
                  ),
                  
                  # Second tab: Facet Grid tab (Commented out to disable it)
                  # tabPanel(
                  #   "Facet Grid",
                  #   tags$strong("Order Class YoY Trends"),
                  #   fluidRow(
                  #     column(width = 12,
                  #            div(class = "plot-container", plotOutput("linePlot")))
                  #   )
                  # )
      )  
    )  
  )  
)



































