#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define UI for application that draws a histogram

fluidPage(
  
  # app title
  titlePanel("Revenue and Economic Indicators"),
  
  sidebarLayout(
    sidebarPanel(
      # select an order class from the data or All
      radioButtons("S_Cons_Order_Class",
                   label = "Select an Order Class", 
                   choices = c("All Sales", sales |> distinct(S_Cons_Order_Class) |> pull(S_Cons_Order_Class) |> sort())),
      
      # slider for Sales Percentile range
      sliderInput("slider2", 
                  label = "Sales Percentile Range", 
                  min = 0, 
                  max = 100, 
                  value = c(10, 90),  # Set default range (e.g., 10th to 90th percentile)
                  step = 1, 
                  animate = TRUE),
      
      # Conditional dropdown for x-axis var in scatter plot
      conditionalPanel(
        condition = "input.tabs == 'Economic Indicator Comparison'",  # check if Economic Indicator Comparison tab is active
        selectInput("scatter_x_var", 
                    label = "Select Variable for X-Axis", 
                    choices = c("New_Jobs", "CCI", "Fed_Funds_Rate", "Jet_Fuel", "Unemployment"))
      ),
      
      # Conditional dropdown for Y-axis variable in scatter plot
      conditionalPanel(
        condition = "input.tabs == 'Economic Indicator Comparison'",  # check if Economic Indicator Comparison tab is active
        selectInput("scatter_y_var", 
                    label = "Select Variable for Y-Axis", 
                    choices = c("Total_Rev", "Total_Rev_Offset1", "Total_Rev_Offset2"))
      )
    ),
    
    # show plots in tabs
    mainPanel(
      tabsetPanel(id = "tabs",  # Assign an id to the tabsetPanel for reference in conditionalPanel
                  
                  # first tab: both dist and scatter
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
                    
                    # table below scatter
                    fluidRow(
                      column(width = 12,
                             tags$strong("Underlying Scatter Plot Data"),
                             DT::dataTableOutput("aggregatedDataTable") 
                      )
                    )
                  ),
                  
                  # New tab: GDP Sector Comparison
                  tabPanel(
                    "GDP Sector Comparison",
                    fluidRow(
                      column(width = 12,
                             div(class = "plot-container", plotOutput("distPlotGDP")))  # Same histogram here
                    )
                  ),
                  
                  # Second tab: corr plots (now placed after GDP tab)
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














