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
  titlePanel("Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      # select an Order Class from the data or All
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
                  animate = TRUE)
    ),
    
    # show plots in tabs
    mainPanel(
      tabsetPanel(
        
        # first tab: both dist and scatter
        tabPanel(
          "Plots", 
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
          )
        ),
        
        # Second tab: Correlation plots
        tabPanel(
          "Correlations",
          h3("Correlation Visualizations"),
          fluidRow(
            column(width = 12,
                   div(class = "plot-container", plotOutput("linePlot")))  # line plot in correlations Tab
          )
        ),
        
        # New tab: Aggregated Data Table
        tabPanel(
          "Aggregated Data",
          h3("Aggregated Sales Data Table"),
          DT::dataTableOutput("aggregatedDataTable")  # Interactive table displaying aggregated data
        )
      )
    )
  )
)




