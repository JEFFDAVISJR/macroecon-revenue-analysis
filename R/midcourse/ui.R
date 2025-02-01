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
  
  # Application title
  titlePanel("App Test"),
  
  sidebarLayout(
    sidebarPanel(
      # Select an Order Class from the data or All
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
                  animate = TRUE)
    ),
    
    # Show plots
    mainPanel(
      fluidRow(
        column(width = 12,
               plotOutput("distPlot"))  
      ),  
      
      fluidRow(
        column(width = 12,
               plotOutput("linePlot"))  
      )  
    )  
  )  
)
