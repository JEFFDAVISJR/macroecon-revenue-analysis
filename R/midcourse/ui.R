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
      radioButtons("OrderClass",
                   label = "Select an Order Class", 
                   choices = c("All", sales |> distinct(S_Cons_Order_Class) |> pull(S_Cons_Order_Class) |> sort()))
    ),  
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(width = 12,
               plotOutput("distPlot")) 
      )
    )
  )
)